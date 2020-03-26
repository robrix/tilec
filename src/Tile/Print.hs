{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Print
( prettyPrint
, prettyPrintWith
, defaultStyle
, runPrint
, Print(..)
, Highlight(..)
, Doc
) where

import           Control.Applicative ((<**>))
import           Control.Carrier.Fresh.Church
import           Control.Carrier.State.Church
import           Control.Carrier.Writer.Church
import           Control.Monad (guard)
import           Control.Monad.IO.Class
import           Data.Function (on)
import           Data.Functor.Identity
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Pretty hiding (Doc, PrecDoc)
import qualified Tile.Pretty as P
import           Tile.Syntax

prettyPrint :: MonadIO m => Print -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> Print -> m ()
prettyPrintWith style  = putDoc . PP.reAnnotate style . getDoc . runPrint

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


getDoc :: Doc -> PP.Doc (Highlight Int)
getDoc (Doc doc) = rainbow (runPrec (Level 0) doc)

newtype Doc = Doc (Prec (Rainbow (PP.Doc (Highlight Int))))
  deriving newtype (P.Doc (Highlight Int), Monoid, P.PrecDoc (Highlight Int), Semigroup, Show)


data V = V
  { vvar :: {-# UNPACK #-} !Int
  , vdoc :: !Doc
  }

instance Eq V where (==) = (==) `on` vvar
instance Ord V where compare = compare `on` vvar
instance Show V where showsPrec p = showsPrec p . vdoc


runPrint :: Print -> Doc
runPrint = run . runWriter (const pure) . evalFresh 0 . evalState Nothing . getAp . runPrintC

newtype Print = Print { runPrintC :: Ap (StateC (Maybe Ctx) (FreshC (WriterC IntSet.IntSet Identity))) Doc }
  deriving (P.Doc (Highlight Int), Monoid, P.PrecDoc (Highlight Int), Semigroup)

instance Show Print where
  showsPrec p = showsPrec p . runPrint

instance Let Print where
  let' (tm ::: ty) b = inContext Let . bind id b $ \ v b ->
    -- FIXME: bind variables on the lhs when tm is a lambda
    kw "let" <+> prettyBind v <+> group (align (prettyAnn (op "=" <+> tm ::: ty))) <+> kw "in" </> b

instance Lam Print where
  lam b = prec (Level 6) . inContext Lam . bind id b $ \ v b ->
    prettyBind v <+> b

  f $$  a = prec (Level 10) (inContext App (f </> prec (Level 11) a))

instance ILam Print where
  ilam b = prec (Level 6) . inContext Lam . bind id b $ \ v b ->
    braces (prettyBind v) <+> b

  f $$? a = prec (Level 10) (inContext App (f </> braces a))

instance Type Print where
  type' = inContext Type (annotate TypeName (pretty "Type"))

  t ->> b = prec (Level 6) . inContext Pi . bind id b $ \ v b ->
    group (align (maybe (prec (Level 7) t) (group . align . parens . prettyAnn . (::: t) . Print . pure . vdoc) v </> op "→" <+> b))

instance IType Print where
  t =>> b = prec (Level 6) . inContext Pi . bind id b $ \ v b ->
    group (align (maybe (braces t) (group . align . braces . prettyAnn . (::: t) . Print . pure . vdoc) v </> op "→" <+> b))

instance Prob Print where
  ex t b = prec (Level 6) . inContext Exists . bind toMeta b $ \ v b ->
    group (align (op "∃" <+> group (align (resetPrec (Level 0) (prettyAnn (prettyBind (toMeta <$> v) ::: t)))) <+> op "." </> resetPrec (Level 0) b)) where
    toMeta v = v { vdoc = annotate MetaVar (pretty '?' <> vdoc v) }

  t1 === t2 = prec (Level 4) (inContext Equate (group (align (flatAlt (space <> space) mempty <> prec (Level 5) (prettyAnn t1) </> op "≡" <+> prec (Level 5) (prettyAnn t2)))))

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

transition :: Maybe Ctx -> Maybe Ctx -> Print -> Print
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
    Just Lam -> (op "." </>) . resetPrec (Level 0)
    _ -> id

inContext :: Ctx -> Print -> Print
inContext ctx m = Print $ do
  ctx' <- get
  if ctx' == Just ctx then
    runPrintC m
  else do
    put (Just ctx)
    a <- runPrintC (transition ctx' (Just ctx) m)
    a <$ put ctx'


kw :: P.Doc (Highlight Int) doc => String -> doc
kw = annotate Keyword . pretty

op :: P.Doc (Highlight Int) doc => String -> doc
op = annotate Op . pretty

prettyBind :: Maybe V -> Print
prettyBind = maybe (pretty '_') (Print . pure . vdoc)

prettyVar :: P.Doc (Highlight Int) doc => Int -> doc
prettyVar i = annotate Name (pretty (alphabet !! r) <> if q > 0 then pretty q else mempty) where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']

prettyAnn :: P.PrecDoc (Highlight Int) doc => doc ::: doc -> doc
prettyAnn (tm ::: ty) = group (prec (Level 6) tm </> group (align (op ":" <+> prec (Level 6) ty)))

bind :: (V -> V) -> (Print -> Print) -> (Maybe V -> Print -> Print) -> Print
bind t b f = Print $ do
  v <- fresh
  let v' = t $ V v (prettyVar v)
  (fvs, b') <- censor (IntSet.delete v) (listen (runPrintC (b (var v'))))
  runPrintC (f (v' <$ guard (v `IntSet.member` fvs)) (Print (pure b')))
  where
  var v = inContext Var (Print (vdoc v <$ tell (IntSet.singleton (vvar v))))
