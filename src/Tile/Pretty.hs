{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Pretty
( putDoc
, Doc(..)
, line
, line'
, enclose
, surround
, encloseSep
, cat
, vcat
, sep
, vsep
, concatWith
, (<+>)
, (</>)
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
import           Data.Monoid (Ap(..))
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

  align :: doc -> doc

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

  annotate = PP.annotate

  align = PP.align

  group = PP.group

  flatAlt = PP.flatAlt

instance (Doc ann a, Doc ann b) => Doc ann (a, b) where
  pretty = pretty &&& pretty

  annotate a = annotate a *** annotate a

  align = align *** align

  group = group *** group

  flatAlt d = flatAlt (fst d) *** flatAlt (snd d)

instance (Applicative f, Doc ann a) => Doc ann (Ap f a) where
  pretty = pure . pretty

  annotate = fmap . annotate

  align = fmap align

  group = fmap group

  flatAlt = liftA2 flatAlt

  parens = fmap parens

  brackets = fmap brackets

  braces = fmap braces

line :: Doc ann doc => doc
line = flatAlt line (pretty ' ')

line' :: Doc ann doc => doc
line' = flatAlt line mempty

enclose :: Doc ann doc => doc -> doc -> doc -> doc
enclose l r x = l <> x <> r

surround :: Doc ann doc => doc -> doc -> doc -> doc
surround x l r = enclose l r x

encloseSep :: Doc ann doc => doc -> doc -> doc -> [doc] -> doc
encloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> cat (zipWith (<>) (l : repeat s) ds) <> r

cat :: Doc ann doc => [doc] -> doc
cat = group . vcat

vcat :: Doc ann doc => [doc] -> doc
vcat = concatWith (surround line')

sep :: Doc ann doc => [doc] -> doc
sep = group . vsep

vsep :: Doc ann doc => [doc] -> doc
vsep = concatWith (</>)

concatWith :: (Doc ann doc, Foldable t) => (doc -> doc -> doc) -> t doc -> doc
concatWith (<>) ds
  | null ds   = mempty
  | otherwise = foldr1 (<>) ds

(<+>) :: Doc ann doc => doc -> doc -> doc
(<+>) = surround (pretty ' ')

infixr 6 <+>

(</>) :: Doc ann doc => doc -> doc -> doc
(</>) = surround line

infixr 6 </>

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

  annotate = fmap . annotate

  align = fmap align

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

  annotate = fmap . annotate

  align = fmap align

  group = fmap group

  flatAlt = liftA2 flatAlt

  parens = fmap parens

  brackets = fmap brackets

  braces = fmap braces

instance Doc ann doc => PrecDoc ann (Prec doc) where
  prec l (Prec d) = Prec $ \ l' -> parensIf (l' > l) (d l)

instance (Applicative f, PrecDoc ann a) => PrecDoc ann (Ap f a) where
  prec = fmap . prec
