{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module S.Syntax.Pretty
( prettyPrint
, PrettyC(..)
, putDoc
) where

import           Control.Monad.IO.Class
import           Data.Semigroup (Last(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           S.Syntax
import           S.Syntax.Classes
import           System.Console.Terminal.Size as Size
import           System.IO (stdout)

prettyPrint :: MonadIO m => PrettyC -> m ()
prettyPrint (PrettyC run) = putDoc (snd (run (Last 0)))

newtype PrettyC = PrettyC { runPrettyC :: Last Int -> (Last Int, PP.Doc ANSI.AnsiStyle) }
  deriving (Semigroup)

instance Show PrettyC where
  showsPrec p (PrettyC run) = showsPrec p (snd (run (Last 0)))

instance Var Int PrettyC where
  var = word . (PP.pretty '_' <>) . PP.pretty

instance Let Int PrettyC where
  let' (tm ::: ty) b = fresh (\ v -> kw "let" <+> var v <+> op "=" <+> tm <+> op ":" <+> ty <+> kw "in" <+> b v)

instance Lam Int PrettyC where
  lam f  = fresh $ \ v -> op "\\" <+> var v <+> op "." <+> f v
  f $$ a = f <+> a

instance Type Int PrettyC where
  type' = kw "Type"
  pi' t f = fresh $ \ v -> parens (var v <+> op ":" <+> t) <+> op "->" <+> f v


word :: PP.Doc ANSI.AnsiStyle -> PrettyC
word s = PrettyC (, s)

kw :: String -> PrettyC
kw = word . PP.pretty

op :: String -> PrettyC
op = word . PP.pretty

(<+>) :: PrettyC -> PrettyC -> PrettyC
l <+> r = l <> word (PP.pretty ' ') <> r

infixr 6 <+>

parens :: PrettyC -> PrettyC
parens c = kw "(" <> c <> kw ")"

fresh :: (Int -> PrettyC) -> PrettyC
fresh f = PrettyC $ \ v -> runPrettyC (f (getLast v)) ((1 +) <$> v)


putDoc :: MonadIO m => PP.Doc ANSI.AnsiStyle -> m ()
putDoc doc = do
  s <- maybe 80 Size.width <$> liftIO size
  liftIO (ANSI.renderIO stdout (PP.layoutSmart PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine s 0.8 } (doc <> PP.line)))
