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

import           Control.Monad.IO.Class
import           Data.Semigroup (Last(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           S.Pretty
import           S.Syntax
import           S.Syntax.Classes

prettyPrint :: MonadIO m => PrettyC -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight -> ANSI.AnsiStyle) -> PrettyC -> m ()
prettyPrintWith style  = putDoc . PP.reAnnotate style . toDoc

defaultStyle :: Highlight -> ANSI.AnsiStyle
defaultStyle = \case
  Var -> mempty
  Op -> ANSI.color ANSI.Cyan
  Type -> ANSI.color ANSI.Yellow
  Keyword -> ANSI.color ANSI.Magenta

newtype PrettyC = PrettyC { runPrettyC :: Last Int -> (Last Int, PP.Doc Highlight) }
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
  f $$ a = f <+> a

instance Type Int PrettyC where
  type' = annotate Type (pretty "Type")
  pi' t f = fresh $ \ v -> parens (var v <+> op ":" <+> t) <+> op "->" <+> f v


data Highlight
  = Var
  | Op
  | Type
  | Keyword
  deriving (Eq, Ord, Show)

kw :: String -> PrettyC
kw = annotate Keyword . pretty

op :: String -> PrettyC
op = annotate Op . pretty

instance Doc Highlight PrettyC where
  pretty = PrettyC . flip (,) . pretty

  annotate h (PrettyC run) = PrettyC (fmap (annotate h) . run)

  l <+> r = l <> PrettyC (, pretty ' ') <> r

  parens c = kw "(" <> c <> kw ")"

  brackets c = kw "[" <> c <> kw "]"

toDoc :: PrettyC -> PP.Doc Highlight
toDoc (PrettyC run) = snd (run (Last 0))

fresh :: (Int -> PrettyC) -> PrettyC
fresh f = PrettyC $ \ v -> runPrettyC (f (getLast v)) ((1 +) <$> v)
