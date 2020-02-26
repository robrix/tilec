{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Pretty
( putDoc
, Doc(..)
, enclose
, rainbow
, Rainbow(..)
) where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&), (***))
import           Control.Monad.IO.Class
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           System.Console.Terminal.Size as Size
import           System.IO (stdout)

putDoc :: MonadIO m => PP.Doc ANSI.AnsiStyle -> m ()
putDoc doc = do
  s <- maybe 80 Size.width <$> liftIO size
  liftIO (ANSI.renderIO stdout (PP.layoutSmart PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine s 0.8 } (doc <> PP.line)))


class Monoid doc => Doc ann doc | doc -> ann where
  pretty :: PP.Pretty a => a -> doc

  annotate :: ann -> doc -> doc

  (<+>) :: doc -> doc -> doc
  infixr 6 <+>

  parens :: doc -> doc

  brackets :: doc -> doc

  braces :: doc -> doc

instance Doc ann (PP.Doc ann) where
  pretty = PP.pretty

  annotate = PP.annotate

  (<+>) = (PP.<+>)

  parens = PP.parens

  brackets = PP.brackets

  braces = PP.braces

instance (Doc ann a, Doc ann b) => Doc ann (a, b) where
  pretty = pretty &&& pretty

  annotate a = annotate a *** annotate a

  (a1, b1) <+> (a2, b2) = (a1 <+> a2, b1 <+> b2)

  parens = parens *** parens

  brackets = brackets *** brackets

  braces = braces *** braces

enclose :: Doc ann doc => doc -> doc -> doc -> doc
enclose l r x = l <> x <> r


rainbow :: Rainbow doc -> doc
rainbow = (`runRainbow` 0)

newtype Rainbow doc = Rainbow { runRainbow :: Int -> doc }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Doc ann doc => Doc ann (Rainbow doc) where
  pretty = Rainbow . const . pretty

  annotate = fmap . annotate

  (<+>) = liftA2 (<+>)

  -- FIXME: what do we need to annotate accordingly? Enum instance maybe?
  parens (Rainbow run) = Rainbow $ \ l -> pretty '(' <> run (1 + l) <> pretty ')'

  brackets (Rainbow run) = Rainbow $ \ l -> pretty '[' <> run (1 + l) <> pretty ']'

  braces (Rainbow run) = Rainbow $ \ l -> pretty '{' <> run (1 + l) <> pretty '}'
