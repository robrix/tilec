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

import           Control.Applicative ((<**>))
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Pretty
import           Tile.Syntax
import           Tile.Type

prettyPrint :: MonadIO m => Print Inner -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> Print Inner -> m ()
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

newtype Print a = Print { runPrint :: Ap (FreshC (WriterC IntSet.IntSet Identity)) a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Show (Print Inner) where
  showsPrec p = showsPrec p . toDoc

instance Var Int (Print Inner) where
  var a = Print (prettyVar a <$ tell (IntSet.singleton a))

instance Let Int (Print Inner) where
  let' tm b = Print $ do
    tm' <- runPrint tm
    (lhs, b') <- bind (runPrint . b) prettyVar (pretty '_')
    -- FIXME: bind variables on the lhs when tm is a lambda
    pure (group (align (kw "let" <+> lhs <+> align (group (align (op "=" <+> tm'))) <> line <> kw "in" <+> b')))

instance Lam Int (Print Inner) where
  lam b  = Print $ do
    (lhs, b') <- bind (runPrint . b) prettyVar (pretty '_')
    -- FIXME: combine successive lambdas into a single \ … . …
    pure (prec (Level 0) (align (op "\\" <+> lhs <+> op "." <> line <> b')))
  -- FIXME: combine successive applications for purposes of wrapping

  f $$ a = Print $ do
    f' <- runPrint f
    a' <- runPrint a
    pure (prec (Level 10) (f' <+> prec (Level 11) a'))

instance Type Int (Print Inner) where
  type' = Print (pure (annotate Type (pretty "Type")))

  t >-> b = Print $ do
    t' <- runPrint t
    (lhs, b') <- bind (runPrint . b) (\ v -> parens (prettyVar v <+> op ":" <+> t')) (prec (Level 1) t')
    pure (prec (Level 0) (lhs <> line <> op "→" <+> b'))

instance Prob Int (Print Inner) where
  ex t b = Print $ do
    t' <- runPrint t
    (lhs, b') <- bind (runPrint . b) prettyVar (pretty '_')
    pure (prec (Level 0) (pretty '∃' <+> lhs <+> op ":" <+> t' <+> op "." <+> b'))

  (tm1 ::: ty1) === (tm2 ::: ty2) = Print $ do
    tm1' <- runPrint tm1
    ty1' <- runPrint ty1
    tm2' <- runPrint tm2
    ty2' <- runPrint ty2
    pure (prec (Level 4) (tm1' <+> op ":" <+> ty1' <+> op "≡" <+> tm2' <+> op ":" <+> ty2'))

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

bind :: (Has Fresh sig m, Has (Writer IntSet.IntSet) sig m) => (Int -> m a) -> (Int -> Inner) -> Inner -> m (Inner, a)
bind b used unused = do
  v <- fresh
  (fvs, b') <- listen @IntSet.IntSet (b v)
  pure (if v `IntSet.member` fvs then used v else unused, b')

toDoc :: Print Inner -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (snd (run (runWriter (evalFresh 0 (getAp m))))) (Level 0))
