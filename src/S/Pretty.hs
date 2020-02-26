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


class Doc doc where
  pretty :: PP.Pretty a => a -> doc ann

  annotate :: ann -> doc ann -> doc ann

  (<+>) :: doc ann -> doc ann -> doc ann

  infixr 6 <+>

  parens :: doc ann -> doc ann

instance Doc PP.Doc where
  pretty = PP.pretty

  annotate = PP.annotate

  (<+>) = (PP.<+>)

  parens = PP.parens
