{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
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

deriving instance Doc     (Highlight Int) (Print Inner)
deriving instance PrecDoc (Highlight Int) (Print Inner)

instance Show (Print Inner) where
  showsPrec p = showsPrec p . toDoc

instance Var Int (Print Inner) where
  var a = Print (prettyVar a <$ tell (IntSet.singleton a))

instance Let Int (Print Inner) where
  let' (tm ::: ty) b = do
    tm' <- tm
    ty' <- ty
    (lhs, b') <- bind b prettyVar (pretty '_')
    -- FIXME: bind variables on the lhs when tm is a lambda
    pure (group (align (kw "let" <+> lhs <+> align (group (align (op "=" <+> tm'))) <> line <> align (group (align (op ":" <+> ty'))) <> line <> kw "in" <+> b')))

instance Lam Int (Print Inner) where
  lam p b  = do
    (lhs, b') <- case p of
      Im -> bind b (braces . prettyVar) (braces (pretty '_'))
      Ex -> bind b prettyVar (pretty '_')
    -- FIXME: combine successive lambdas into a single \ … . …
    pure (prec (Level 0) (align (op "\\" <+> lhs <+> op "." <> line <> b')))
  -- FIXME: combine successive applications for purposes of wrapping

  f $$ a = do
    f' <- f
    a' <- a
    pure (prec (Level 10) (f' <+> prec (Level 11) a'))

instance Type Int (Print Inner) where
  type' = pure (annotate Type (pretty "Type"))

  (p, t) >-> b = do
    t' <- t
    (lhs, b') <- case p of
      Im -> bind b (\ v -> braces (prettyVar v <+> op ":" <+> t')) (braces t')
      Ex -> bind b (\ v -> parens (prettyVar v <+> op ":" <+> t')) (prec (Level 1) t')
    pure (prec (Level 0) (group (lhs <> line <> op "→" <+> b')))

instance Prob Int (Print Inner) where
  ex t b = do
    t' <- t
    (lhs, b') <- bind b prettyVar (pretty '_')
    pure (prec (Level 0) (pretty '∃' <+> lhs <+> op ":" <+> t' <+> op "." <+> b'))

  (tm1 ::: ty1) === (tm2 ::: ty2) = do
    tm1' <- tm1
    ty1' <- ty1
    tm2' <- tm2
    ty2' <- ty2
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

bind :: (Int -> Print a) -> (Int -> Inner) -> Inner -> Print (Inner, a)
bind b used unused = Print $ do
  v <- fresh
  (fvs, b') <- listen @IntSet.IntSet (runPrint (b v))
  pure (if v `IntSet.member` fvs then used v else unused, b')

toDoc :: Print Inner -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (snd (run (runWriter (evalFresh 0 (getAp m))))) (Level 0))
