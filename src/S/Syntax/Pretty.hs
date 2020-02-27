{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module S.Syntax.Pretty
( prettyPrint
, prettyPrintWith
, defaultStyle
, PrettyC(..)
, Highlight(..)
) where

import           Control.Applicative ((<**>))
import           Control.Carrier.Fresh.Strict
import           Control.Effect.Writer
import           Control.Monad.IO.Class
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           S.Pretty
import           S.Syntax
import           S.Syntax.Classes

prettyPrint :: MonadIO m => PrettyC -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> PrettyC -> m ()
prettyPrintWith style  = putDoc . PP.reAnnotate style . toDoc

defaultStyle :: Highlight Int -> ANSI.AnsiStyle
defaultStyle = \case
  Var -> mempty
  Op -> ANSI.color ANSI.Cyan
  Type -> ANSI.color ANSI.Yellow
  Keyword -> ANSI.color ANSI.Magenta
  Nest i -> colours !! (i `mod` len)
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

newtype PrettyC = PrettyC { runPrettyC :: Ap (FreshC ((,) IntSet.IntSet)) Inner }
  deriving (Monoid, Semigroup)

instance Show PrettyC where
  showsPrec p = showsPrec p . toDoc

instance Var Int PrettyC where
  var a = PrettyC (annotate Var (pretty '_' <> pretty a) <$ tell (IntSet.singleton a))

instance Let Int PrettyC where
  let' (tm ::: ty) b = PrettyC $ do
    v <- fresh
    runPrettyC (kw "let" <+> var v <+> op "=" <+> tm <+> op ":" <+> ty <+> kw "in" <+> b v)

instance Lam Int PrettyC where
  lam f  = PrettyC $ do
    v <- fresh
    runPrettyC (op "\\" <+> var v <+> op "." <+> f v)
  f $$ a = prec (Level 10) (f <+> prec (Level 11) a)

instance Type Int PrettyC where
  type' = annotate Type (pretty "Type")
  pi' t f = PrettyC $ do
    v <- fresh
    runPrettyC (prec (Level 0) (parens (var v <+> op ":" <+> t) <+> op "->" <+> f v))


data Highlight a
  = Var
  | Op
  | Type
  | Keyword
  | Nest a
  deriving (Eq, Functor, Ord, Show)

instance Applicative Highlight where
  pure = Nest
  f <*> a = case f of
    Var     -> Var
    Op      -> Op
    Type    -> Type
    Keyword -> Keyword
    Nest f  -> f <$> a


kw :: String -> PrettyC
kw = annotate Keyword . pretty

op :: String -> PrettyC
op = annotate Op . pretty

instance Doc (Highlight Int) PrettyC where
  pretty = PrettyC . pure . pretty

  annotate = mapDoc . annotate

  parens = mapDoc parens

  brackets = mapDoc brackets

  braces = mapDoc braces

instance PrecDoc (Highlight Int) PrettyC where
  prec = mapDoc . prec

mapDoc :: (Inner -> Inner) -> PrettyC -> PrettyC
mapDoc f (PrettyC run) = PrettyC (f <$> run)

toDoc :: PrettyC -> PP.Doc (Highlight Int)
toDoc (PrettyC m) = rainbow (runPrec (snd (evalFresh 0 (getAp m))) (Level 0))
