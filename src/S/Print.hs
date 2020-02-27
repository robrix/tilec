{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module S.Print
( prettyPrint
, prettyPrintWith
, defaultStyle
, PrettyC(..)
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
import           S.Pretty
import           S.Syntax
import           S.Type

prettyPrint :: MonadIO m => PrettyC -> m ()
prettyPrint = prettyPrintWith defaultStyle

prettyPrintWith :: MonadIO m => (Highlight Int -> ANSI.AnsiStyle) -> PrettyC -> m ()
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

newtype PrettyC = PrettyC { runPrettyC :: M Inner }
  deriving (Monoid, Semigroup)

instance Show PrettyC where
  showsPrec p = showsPrec p . toDoc

instance Var Int PrettyC where
  var a = PrettyC (prettyVar a <$ tell (IntSet.singleton a))

instance Let Int PrettyC where
  let' (tm ::: ty) b = PrettyC $ do
    tm' <- runPrettyC tm
    ty' <- runPrettyC ty
    (lhs, b') <- bind b prettyVar (pretty '_')
    -- FIXME: bind variables on the lhs when tm is a lambda
    pure (group (align (kw "let" <+> lhs <+> align (group (align (op "=" <+> tm')) <> line <> group (align (op ":" <+> ty'))) <> line <> kw "in" <+> b')))

instance Lam Int PrettyC where
  lam b  = PrettyC $ do
    (lhs, b') <- bind b prettyVar (pretty '_')
    -- FIXME: combine successive lambdas into a single \ … . …
    pure (prec (Level 0) (align (op "\\" <+> lhs <+> op "." <> line <> b')))
  -- FIXME: combine successive applications for purposes of wrapping
  f $$ a = prec (Level 10) (f <+> prec (Level 11) a)

instance Type Int PrettyC where
  type' = annotate Type (pretty "Type")
  pi' t b = PrettyC $ do
    t' <- runPrettyC t
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

bind :: (Int -> PrettyC) -> (Int -> Inner) -> Inner -> M (Inner, Inner)
bind b used unused = do
  v <- fresh
  (fvs, b') <- listen (runPrettyC (b v))
  pure (if v `IntSet.member` fvs then used v else unused, b')

instance Doc (Highlight Int) PrettyC where
  pretty = coerce . pure @M . pretty

  line = coerce (pure @M line)

  annotate = coerce . fmap @M . annotate

  align = coerce (fmap @M align)

  group = coerce (fmap @M group)

  flatAlt = coerce (liftA2 @M flatAlt)

  parens = coerce (fmap @M parens)

  brackets = coerce (fmap @M brackets)

  braces = coerce (fmap @M braces)

instance PrecDoc (Highlight Int) PrettyC where
  prec = coerce . fmap @M . prec

toDoc :: PrettyC -> PP.Doc (Highlight Int)
toDoc (PrettyC m) = rainbow (runPrec (snd (evalFresh 0 (getAp m))) (Level 0))
