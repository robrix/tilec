{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Tile.Print
( prettyPrint
, prettyPrintWith
, defaultStyle
, Print(..)
, Highlight(..)
) where

import           Control.Applicative (liftA2, (<**>))
import           Control.Carrier.Fresh.Strict
import           Control.Effect.Writer
import           Control.Monad.IO.Class
import           Data.Coerce (coerce)
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Pretty
import           Tile.Syntax
import           Tile.Type

prettyPrint :: MonadIO m => Print -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> Print -> m ()
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

type M = Ap (FreshC ((,) IntSet.IntSet))

newtype Print = Print { runPrint :: M Inner }
  deriving (Monoid, Semigroup)

instance Show Print where
  showsPrec p = showsPrec p . toDoc

instance Var Int Print where
  var a = Print (prettyVar a <$ tell (IntSet.singleton a))

instance Let Int Print where
  let' (tm ::: ty) b = Print $ do
    tm' <- runPrint tm
    ty' <- runPrint ty
    (lhs, b') <- bind b prettyVar (pretty '_')
    -- FIXME: bind variables on the lhs when tm is a lambda
    pure (group (align (kw "let" <+> lhs <+> align (group (align (op "=" <+> tm')) <> line <> group (align (op ":" <+> ty'))) <> line <> kw "in" <+> b')))

instance Lam Int Print where
  lam b  = Print $ do
    (lhs, b') <- bind b prettyVar (pretty '_')
    -- FIXME: combine successive lambdas into a single \ … . …
    pure (prec (Level 0) (align (op "\\" <+> lhs <+> op "." <> line <> b')))
  -- FIXME: combine successive applications for purposes of wrapping
  f $$ a = prec (Level 10) (f <+> prec (Level 11) a)

instance Type Int Print where
  type' = annotate Type (pretty "Type")
  pi' t b = Print $ do
    t' <- runPrint t
    (lhs, b') <- bind b (\ v -> parens (prettyVar v <+> op ":" <+> t')) (prec (Level 1) t')
    pure (prec (Level 0) (lhs <> line <> op "→" <+> b'))


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


kw :: Doc (Highlight Int) doc => String -> doc
kw = annotate Keyword . pretty

op :: Doc (Highlight Int) doc => String -> doc
op = annotate Op . pretty

prettyVar :: Doc (Highlight Int) doc => Int -> doc
prettyVar i = annotate Var (pretty (alphabet !! r) <> if q > 0 then pretty q else mempty) where
  (q, r) = i `divMod` 26
  alphabet = ['a'..'z']

bind :: (Int -> Print) -> (Int -> Inner) -> Inner -> M (Inner, Inner)
bind b used unused = do
  v <- fresh
  (fvs, b') <- listen (runPrint (b v))
  pure (if v `IntSet.member` fvs then used v else unused, b')

instance Doc (Highlight Int) Print where
  pretty = coerce . pure @M . pretty

  line = coerce (pure @M line)

  annotate = coerce . fmap @M . annotate

  align = coerce (fmap @M align)

  group = coerce (fmap @M group)

  flatAlt = coerce (liftA2 @M flatAlt)

  parens = coerce (fmap @M parens)

  brackets = coerce (fmap @M brackets)

  braces = coerce (fmap @M braces)

instance PrecDoc (Highlight Int) Print where
  prec = coerce . fmap @M . prec

toDoc :: Print -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (snd (evalFresh 0 (getAp m))) (Level 0))
