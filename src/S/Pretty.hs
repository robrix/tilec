{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Pretty
( putDoc
, Doc(..)
, enclose
, (<+>)
, Level(..)
, rainbow
, Rainbow(..)
, Prec(..)
) where

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

  parens :: doc -> doc
  parens = enclose (pretty "(") (pretty ")")

  brackets :: doc -> doc
  brackets = enclose (pretty "[") (pretty "]")

  braces :: doc -> doc
  braces = enclose (pretty "{") (pretty "}")

instance Doc ann (PP.Doc ann) where
  pretty = PP.pretty

  annotate = PP.annotate

instance (Doc ann a, Doc ann b) => Doc ann (a, b) where
  pretty = pretty &&& pretty

  annotate a = annotate a *** annotate a

enclose :: Doc ann doc => doc -> doc -> doc -> doc
enclose l r x = l <> x <> r

(<+>) :: Doc ann doc => doc -> doc -> doc
l <+> r = enclose l r (pretty ' ')

infixr 6 <+>


newtype Level = Level Int
  deriving (Eq, Ord, Show)


rainbow :: Rainbow doc -> doc
rainbow = (`runRainbow` 0)

newtype Rainbow doc = Rainbow { runRainbow :: Int -> doc }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance (Doc (ann Int) doc, Applicative ann) => Doc (ann Int) (Rainbow doc) where
  pretty = pure . pretty

  annotate = fmap . annotate

  parens   (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '(') <> run (1 + l) <> annotate (pure l) (pretty ')')
  brackets (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '[') <> run (1 + l) <> annotate (pure l) (pretty ']')
  braces   (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '{') <> run (1 + l) <> annotate (pure l) (pretty '}')


newtype Prec a = Prec { runPrec :: Level -> a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Doc ann doc => Doc ann (Prec doc) where
  pretty = pure . pretty

  annotate = fmap . annotate

  parens = fmap parens

  brackets = fmap brackets

  braces = fmap braces
