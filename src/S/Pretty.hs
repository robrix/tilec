{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Pretty
( putDoc
, Doc(..)
, line'
, enclose
, surround
, cat
, vcat
, concatWith
, (<+>)
, parensIf
, Level(..)
, PrecDoc(..)
, rainbow
, Rainbow(..)
, Prec(..)
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

  line :: doc

  annotate :: ann -> doc -> doc

  group :: doc -> doc

  flatAlt :: doc -> doc -> doc

  parens :: doc -> doc
  parens = enclose (pretty "(") (pretty ")")

  brackets :: doc -> doc
  brackets = enclose (pretty "[") (pretty "]")

  braces :: doc -> doc
  braces = enclose (pretty "{") (pretty "}")

instance Doc ann (PP.Doc ann) where
  pretty = PP.pretty

  line = PP.line

  annotate = PP.annotate

  group = PP.group

  flatAlt = PP.flatAlt

instance (Doc ann a, Doc ann b) => Doc ann (a, b) where
  pretty = pretty &&& pretty

  line = (line, line)

  annotate a = annotate a *** annotate a

  group = group *** group

  flatAlt d = flatAlt (fst d) *** flatAlt (snd d)

line' :: Doc ann doc => doc
line' = flatAlt line mempty

enclose :: Doc ann doc => doc -> doc -> doc -> doc
enclose l r x = l <> x <> r

surround :: Doc ann doc => doc -> doc -> doc -> doc
surround x l r = enclose l r x

cat :: Doc ann doc => [doc] -> doc
cat = group . vcat

vcat :: Doc ann doc => [doc] -> doc
vcat = concatWith (surround line')

concatWith :: (Doc ann doc, Foldable t) => (doc -> doc -> doc) -> t doc -> doc
concatWith (<>) ds
  | null ds   = mempty
  | otherwise = foldr1 (<>) ds

(<+>) :: Doc ann doc => doc -> doc -> doc
l <+> r = enclose l r (pretty ' ')

infixr 6 <+>

parensIf :: Doc ann doc => Bool -> doc -> doc
parensIf True = parens
parensIf _    = id


newtype Level = Level Int
  deriving (Eq, Ord, Show)

class Doc ann doc => PrecDoc ann doc where
  prec :: Level -> doc -> doc


rainbow :: Rainbow doc -> doc
rainbow = (`runRainbow` 0)

newtype Rainbow doc = Rainbow { runRainbow :: Int -> doc }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance (Doc (ann Int) doc, Applicative ann) => Doc (ann Int) (Rainbow doc) where
  pretty = pure . pretty

  line = pure line

  annotate = fmap . annotate

  group = fmap group

  flatAlt = liftA2 flatAlt

  parens   (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '(') <> run (1 + l) <> annotate (pure l) (pretty ')')
  brackets (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '[') <> run (1 + l) <> annotate (pure l) (pretty ']')
  braces   (Rainbow run) = Rainbow $ \ l -> annotate (pure l) (pretty '{') <> run (1 + l) <> annotate (pure l) (pretty '}')

instance (PrecDoc (ann Int) doc, Applicative ann) => PrecDoc (ann Int) (Rainbow doc) where
  prec = fmap . prec


newtype Prec a = Prec { runPrec :: Level -> a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Doc ann doc => Doc ann (Prec doc) where
  pretty = pure . pretty

  line = pure line

  annotate = fmap . annotate

  group = fmap group

  flatAlt = liftA2 flatAlt

  parens = fmap parens

  brackets = fmap brackets

  braces = fmap braces

instance Doc ann doc => PrecDoc ann (Prec doc) where
  prec l (Prec d) = Prec $ \ l' -> parensIf (l' > l) (d l)
