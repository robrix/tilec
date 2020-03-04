{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Print
( prettyPrint
, prettyPrintWith
, defaultStyle
, Print(..)
, Highlight(..)
) where

import           Control.Applicative ((<**>))
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad (guard)
import           Control.Monad.IO.Class
import           Data.Function (on)
import           Data.Functor.Identity
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Pretty
import           Tile.Syntax

prettyPrint :: MonadIO m => Print Inner -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> Print Inner -> m ()
prettyPrintWith style  = putDoc . PP.reAnnotate style . toDoc

defaultStyle :: Highlight Int -> ANSI.AnsiStyle
defaultStyle = \case
  Name     -> mempty
  Op       -> ANSI.color ANSI.Cyan
  TypeName -> ANSI.color ANSI.Yellow
  Keyword  -> ANSI.color ANSI.Magenta
  MetaVar  -> ANSI.colorDull ANSI.Black <> ANSI.bold
  Error    -> ANSI.color ANSI.Red
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

type Inner = Prec (Rainbow (PP.Doc (Highlight Int)))

data V = V
  { vvar :: {-# UNPACK #-} !Int
  , vdoc :: !Inner
  }

instance Eq V where (==) = (==) `on` vvar
instance Ord V where compare = compare `on` vvar
instance Show V where showsPrec p = showsPrec p . rainbow . (`runPrec` Level 0) . vdoc

newtype Print a = Print { runPrint :: Ap (StateC (Maybe Ctx) (FreshC (WriterC IntSet.IntSet Identity))) a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

deriving instance Doc     (Highlight Int) (Print Inner)
deriving instance PrecDoc (Highlight Int) (Print Inner)

instance Show (Print Inner) where
  showsPrec p = showsPrec p . toDoc

instance Var V (Print Inner) where
  var v = inContext Var (Print (vdoc v <$ tell (IntSet.singleton (vvar v))))

instance Let V (Print Inner) where
  let' (tm ::: ty) b = inContext Let . bind b $ \ v b ->
    -- FIXME: bind variables on the lhs when tm is a lambda
    kw "let" <+> prettyBind v <+> group (align (prettyAnn (op "=" <+> tm ::: ty))) <+> kw "in" </> b

instance Lam V (Print Inner) where
  lam p b = prec (Level 6) . inContext Lam . bind b $ \ v b ->
    plicit braces id p (prettyBind v) <+> b

  f $$ a = prec (Level 10) (inContext App (f </> prec (Level 11) a))

instance Type V (Print Inner) where
  type' = inContext Type (annotate TypeName (pretty "Type"))

  (p, t) >-> b = prec (Level 6) . inContext Pi . bind b $ \ v b ->
    group (align (maybe (plicit braces (prec (Level 7)) p t) (group . align . plicit braces parens p . prettyAnn . (::: t) . pure . vdoc) v </> op "→" <+> b))

instance Prob V (Print Inner) where
  ex t b = prec (Level 6) . inContext Exists . bind (b . toMeta) $ \ v b ->
    group (align (pretty '∃' <+> group (align (reset (Level 0) (prettyAnn (prettyBind (toMeta <$> v) ::: t)))) <+> op "." </> reset (Level 0) b)) where
    toMeta v = v { vdoc = pretty '?' <> vdoc v }

  t1 === t2 = prec (Level 4) (inContext Equate (group (align (flatAlt (space <> space) mempty <> prec (Level 5) (prettyAnn t1) </> op "≡" <+> prec (Level 5) (prettyAnn t2)))))

instance Err V (Print Inner) where
  err s = annotate Error (pretty "error") <> pretty ':' <+> pretty s

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

transition :: Maybe Ctx -> Maybe Ctx -> Print Inner -> Print Inner
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

inContext :: Ctx -> Print Inner -> Print Inner
inContext ctx m = do
  ctx' <- Print get
  if ctx' == Just ctx then
    m
  else do
    Print (put (Just ctx))
    a <- transition ctx' (Just ctx) m
    a <$ Print (put ctx')


kw :: Doc (Highlight Int) doc => String -> doc
kw = annotate Keyword . pretty

op :: Doc (Highlight Int) doc => String -> doc
op = annotate Op . pretty

prettyBind :: Maybe V -> Print Inner
prettyBind = maybe (pretty '_') (pure . vdoc)

prettyVar :: Doc (Highlight Int) doc => Int -> doc
prettyVar i = annotate Name (pretty (alphabet !! r) <> if q > 0 then pretty q else mempty) where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']

prettyAnn :: PrecDoc (Highlight Int) doc => doc ::: doc -> doc
prettyAnn (tm ::: ty) = group (prec (Level 6) tm </> group (align (op ":" <+> prec (Level 6) ty)))

bind :: (V -> Print a) -> (Maybe V -> Print a -> Print b) -> Print b
bind b f = Print $ do
  v <- fresh
  let v' = V v (prettyVar v)
  (fvs, b') <- censor (IntSet.delete v) (listen (runPrint (b v')))
  runPrint (f (v' <$ guard (v `IntSet.member` fvs)) (pure b'))

toDoc :: Print Inner -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (snd (run (runWriter (evalFresh 0 (evalState Nothing (getAp m)))))) (Level 0))
