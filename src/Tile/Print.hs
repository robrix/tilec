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
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import qualified Data.IntSet as IntSet
import           Data.Monoid (Ap(..))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import           Tile.Pretty
import           Tile.Syntax

prettyPrint :: MonadIO m => Print () -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> Print () -> m ()
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

newtype Print a = Print { runPrint :: Ap (FreshC (WriterC IntSet.IntSet (StateC Inner Identity))) a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Show (Print a) where
  showsPrec p = showsPrec p . toDoc

instance Var Int (Print ()) where
  var a = Print (tell (IntSet.singleton a) *> put @Inner (prettyVar a))

instance Let Int (Print ()) where
  let' tm b = Print $ do
    tm' <- runPrint tm *> get
    (lhs, b') <- bind (runPrint . b) prettyVar (pretty '_')
    -- FIXME: bind variables on the lhs when tm is a lambda
    put @Inner (group (align (kw "let" <+> lhs <+> align (group (align (op "=" <+> tm'))) <> line <> kw "in" <+> b')))

instance Lam Int (Print ()) where
  lam b  = Print $ do
    (lhs, b') <- bind (runPrint . b) prettyVar (pretty '_')
    -- FIXME: combine successive lambdas into a single \ … . …
    put @Inner (prec (Level 0) (align (op "\\" <+> lhs <+> op "." <> line <> b')))
  -- FIXME: combine successive applications for purposes of wrapping
  f $$ a = Print $ do
    f' <- runPrint f *> get
    a' <- runPrint a *> get
    put @Inner (prec (Level 10) (f' <+> prec (Level 11) a'))

instance Type Int (Print ()) where
  type' = Print (put @Inner (annotate Type (pretty "Type")))
  t >-> b = Print $ do
    t' <- runPrint t *> get
    (lhs, b') <- bind (runPrint . b) (\ v -> parens (prettyVar v <+> op ":" <+> t')) (prec (Level 1) t')
    put @Inner (prec (Level 0) (lhs <> line <> op "→" <+> b'))
  tm .:. ty = Print $ do
    tm' <- runPrint tm *> get
    ty' <- runPrint ty *> get
    put @Inner (prec (Level 0) (tm' <+> op ":" <+> prec (Level 1) ty'))


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

bind :: (Has Fresh sig m, Has (State Inner) sig m, Has (Writer IntSet.IntSet) sig m) => (Int -> m a) -> (Int -> Inner) -> Inner -> m (Inner, Inner)
bind b used unused = do
  v <- fresh
  (fvs, b') <- listen @IntSet.IntSet (b v *> get)
  pure (if v `IntSet.member` fvs then used v else unused, b')

toDoc :: Print a -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (run (execState mempty (runWriter (evalFresh 0 (getAp m))))) (Level 0))
