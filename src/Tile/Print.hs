{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Print
( prettyPrint
, prettyPrintWith
, defaultStyle
, runPrint
, PrintC(PrintC)
, Highlight(..)
) where

import           Control.Algebra
import           Control.Applicative ((<**>))
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad (guard, (<=<))
import           Control.Monad.IO.Class
import           Data.Function (on)
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Error
import           Tile.Pretty hiding (Doc, PrecDoc)
import qualified Tile.Pretty as P
import           Tile.Syntax

prettyPrint :: MonadIO m => PrintC m Doc -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> PrintC m Doc -> m ()
prettyPrintWith style  = putDoc . PP.reAnnotate style . toDoc <=< runPrint

defaultStyle :: Highlight Int -> ANSI.AnsiStyle
defaultStyle = \case
  Name     -> mempty
  Op       -> ANSI.color     ANSI.Cyan
  TypeName -> ANSI.color     ANSI.Yellow
  Keyword  -> ANSI.color     ANSI.Magenta
  MetaVar  -> ANSI.colorDull ANSI.Black <> ANSI.bold
  Error    -> ANSI.color     ANSI.Red
  Nest i   -> colours !! (i `mod` len)
  where
  colours =
    [ ANSI.Red
    , ANSI.Green
    , ANSI.Yellow
    , ANSI.Blue
    , ANSI.Magenta
    , ANSI.Cyan
    ]
    <**>
    [ANSI.color, ANSI.colorDull]
  len = length colours


toDoc :: Doc -> PP.Doc (Highlight Int)
toDoc (Doc doc) = rainbow (runPrec (Level 0) doc)

newtype Doc = Doc (Prec (Rainbow (PP.Doc (Highlight Int))))
  deriving newtype (P.Doc (Highlight Int), Monoid, P.PrecDoc (Highlight Int), Semigroup, Show)

instance FreeVariable V Doc where
  freeVariable v = annotate Error (pretty "error") <> pretty ':' <+> vdoc v


data V = V
  { vvar :: {-# UNPACK #-} !Int
  , vdoc :: !Doc
  }

instance Eq V where (==) = (==) `on` vvar
instance Ord V where compare = compare `on` vvar
instance Show V where showsPrec p = showsPrec p . vdoc


runPrint :: Functor m => PrintC m a -> m a
runPrint = fmap snd . runWriter . evalFresh 0 . evalState Nothing . getAp . runPrintC

newtype PrintC m a = PrintC { runPrintC :: Ap (StateC (Maybe Ctx) (FreshC (WriterC IntSet.IntSet m))) a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Algebra sig m => Algebra sig (PrintC m) where
  alg hdl sig = PrintC . alg (runPrintC . hdl) (R (R (R sig)))

deriving instance Monad m => P.Doc     (Highlight Int) (PrintC m Doc)
deriving instance Monad m => P.PrecDoc (Highlight Int) (PrintC m Doc)

instance (Functor m, Show (m Doc)) => Show (PrintC m Doc) where
  showsPrec p = showsPrec p . runPrint

instance Algebra sig m => Var V Doc (PrintC m) where
  var v = inContext Var (PrintC (vdoc v <$ tell (IntSet.singleton (vvar v))))

instance Algebra sig m => Let V Doc (PrintC m) where
  let' (tm ::: ty) b = inContext Let . bind b $ \ v b ->
    -- FIXME: bind variables on the lhs when tm is a lambda
    kw "let" <+> prettyBind v <+> group (align (prettyAnn (op "=" <+> tm ::: ty))) <+> kw "in" </> b

instance Algebra sig m => Lam V Doc (PrintC m) where
  lam p b = prec (Level 6) . inContext Lam . bind b $ \ v b ->
    plicit braces id p (prettyBind v) <+> b

  f $$ a = prec (Level 10) (inContext App (f </> prec (Level 11) a))

instance Algebra sig m => Type V Doc (PrintC m) where
  type' = inContext Type (annotate TypeName (pretty "Type"))

  (p, t) >-> b = prec (Level 6) . inContext Pi . bind b $ \ v b ->
    group (align (maybe (plicit braces (prec (Level 7)) p t) (group . align . plicit braces parens p . prettyAnn . (::: t) . pure . vdoc) v </> op "→" <+> b))

instance Algebra sig m => Prob V Doc (PrintC m) where
  ex t b = prec (Level 6) . inContext Exists . bind (b . toMeta) $ \ v b ->
    group (align (op "∃" <+> group (align (reset (Level 0) (prettyAnn (prettyBind (toMeta <$> v) ::: t)))) <+> op "." </> reset (Level 0) b)) where
    toMeta v = v { vdoc = annotate MetaVar (pretty '?' <> vdoc v) }

  t1 === t2 = prec (Level 4) (inContext Equate (group (align (flatAlt (space <> space) mempty <> prec (Level 5) (prettyAnn t1) </> op "≡" <+> prec (Level 5) (prettyAnn t2)))))

instance Algebra sig m => Err Doc Doc (PrintC m) where
  err = pure

data Highlight a
  = Name
  | Op
  | TypeName
  | Keyword
  | MetaVar
  | Error
  | Nest a
  deriving (Eq, Functor, Ord, Show)

instance Applicative Highlight where
  pure = Nest
  f <*> a = case f of
    Name     -> Name
    Op       -> Op
    TypeName -> TypeName
    Keyword  -> Keyword
    MetaVar  -> MetaVar
    Error    -> Error
    Nest f   -> f <$> a


data Ctx
  = Var
  | Let
  | Lam
  | App
  | Type
  | Pi
  | Exists
  | Equate
  deriving (Eq, Ord, Show)

transition :: Monad m => Maybe Ctx -> Maybe Ctx -> PrintC m Doc -> PrintC m Doc
transition from to = exit from . enter to where
  enter = \case
    Just Let    -> group . align
    Just Lam    -> group . align . (op "\\" <+>)
    Just App    -> group . align
    Just Pi     -> group
    Just Exists -> group
    Just Equate -> group
    _ -> id
  exit = \case
    Just Lam -> (op "." </>) . reset (Level 0)
    _ -> id

inContext :: Algebra sig m => Ctx -> PrintC m Doc -> PrintC m Doc
inContext ctx m = do
  ctx' <- PrintC get
  if ctx' == Just ctx then
    m
  else do
    PrintC (put (Just ctx))
    a <- transition ctx' (Just ctx) m
    a <$ PrintC (put ctx')


kw :: P.Doc (Highlight Int) doc => String -> doc
kw = annotate Keyword . pretty

op :: P.Doc (Highlight Int) doc => String -> doc
op = annotate Op . pretty

prettyBind :: Monad m => Maybe V -> PrintC m Doc
prettyBind = maybe (pretty '_') (pure . vdoc)

prettyVar :: P.Doc (Highlight Int) doc => Int -> doc
prettyVar i = annotate Name (pretty (alphabet !! r) <> if q > 0 then pretty q else mempty) where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']

prettyAnn :: P.PrecDoc (Highlight Int) doc => doc ::: doc -> doc
prettyAnn (tm ::: ty) = group (prec (Level 6) tm </> group (align (op ":" <+> prec (Level 6) ty)))

bind :: Algebra sig m => (V -> PrintC m a) -> (Maybe V -> PrintC m a -> PrintC m b) -> PrintC m b
bind b f = PrintC $ do
  v <- fresh
  let v' = V v (prettyVar v)
  (fvs, b') <- censor (IntSet.delete v) (listen (runPrintC (b v')))
  runPrintC (f (v' <$ guard (v `IntSet.member` fvs)) (pure b'))
