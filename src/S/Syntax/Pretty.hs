{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module S.Syntax.Pretty
( prettyPrint
, prettyPrintWith
, defaultStyle
, PrettyC(..)
, Highlight(..)
) where

import           Control.Applicative ((<**>))
import           Control.Monad.IO.Class
import           Data.Semigroup (Last(..))
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
    , ANSI.White
    ]
    <**>
    [ANSI.color, ANSI.colorDull]
  len = length colours

newtype PrettyC = PrettyC { runPrettyC :: Last Int -> (Last Int, Prec (Rainbow (PP.Doc (Highlight Int)))) }
  deriving (Semigroup)

instance Monoid PrettyC where
  mempty = PrettyC (, mempty)

instance Show PrettyC where
  showsPrec p = showsPrec p . toDoc

instance Var Int PrettyC where
  var = annotate Var . (pretty '_' <>) . pretty

instance Let Int PrettyC where
  let' (tm ::: ty) b = fresh (\ v -> kw "let" <+> var v <+> op "=" <+> tm <+> op ":" <+> ty <+> kw "in" <+> b v)

instance Lam Int PrettyC where
  lam f  = fresh $ \ v -> op "\\" <+> var v <+> op "." <+> f v
  f $$ a = prec (Level 10) (f <+> prec (Level 11) a)

instance Type Int PrettyC where
  type' = annotate Type (pretty "Type")
  pi' t f = fresh $ \ v -> parens (var v <+> op ":" <+> t) <+> op "->" <+> f v


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
  pretty = PrettyC . flip (,) . pretty

  annotate = mapDoc . annotate

  parens = mapDoc parens

  brackets = mapDoc brackets

  braces = mapDoc braces

instance PrecDoc (Highlight Int) PrettyC where
  prec = mapDoc . prec

mapDoc :: (Prec (Rainbow (PP.Doc (Highlight Int))) -> Prec (Rainbow (PP.Doc (Highlight Int)))) -> PrettyC -> PrettyC
mapDoc f (PrettyC run) = PrettyC (fmap f . run)

toDoc :: PrettyC -> PP.Doc (Highlight Int)
toDoc (PrettyC run) = rainbow (runPrec (snd (run (Last 0))) (Level 0))

fresh :: (Int -> PrettyC) -> PrettyC
fresh f = PrettyC $ \ v -> runPrettyC (f (getLast v)) ((1 +) <$> v)
