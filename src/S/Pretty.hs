{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module S.Pretty
( putDoc
, Doc(..)
) where

import           Control.Monad.IO.Class
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           System.Console.Terminal.Size as Size
import           System.IO (stdout)

putDoc :: MonadIO m => PP.Doc ANSI.AnsiStyle -> m ()
putDoc doc = do
  s <- maybe 80 Size.width <$> liftIO size
  liftIO (ANSI.renderIO stdout (PP.layoutSmart PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine s 0.8 } (doc <> PP.line)))


class Doc ann doc | doc -> ann where
  pretty :: PP.Pretty a => a -> doc

  annotate :: ann -> doc -> doc

  (<+>) :: doc -> doc -> doc

  infixr 6 <+>

  parens :: doc -> doc

instance Doc ann (PP.Doc ann) where
  pretty = PP.pretty

  annotate = PP.annotate

  (<+>) = (PP.<+>)

  parens = PP.parens
