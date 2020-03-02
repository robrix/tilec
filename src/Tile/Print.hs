{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import           Control.Monad (guard)
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
  let' (tm ::: ty) b = bind b $ \ v b ->
    -- FIXME: bind variables on the lhs when tm is a lambda
    group (align (kw "let" <+> maybe (pretty '_') prettyVar v <+> align (group (align (op "=" <+> tm))) </> align (group (align (op ":" <+> ty))) </> kw "in" <+> b))

instance Lam Int (Print Inner) where
  lam p b  = bind b $ \ v b ->
    -- FIXME: combine successive lambdas into a single \ … . …
    prec (Level 0) (group (align (op "\\" <+> wrap (maybe (pretty '_') prettyVar v) <+> op "." </> b))) where
    wrap = case p of { Im -> braces ; _ -> id }

  -- FIXME: combine successive applications for purposes of wrapping
  f $$ a = prec (Level 10) (f <+> prec (Level 11) a)

instance Type Int (Print Inner) where
  type' = annotate Type (pretty "Type")

  (p, t) >-> b = bind b $ \ v b ->
    prec (Level 0) (group (maybe (wrap0 t) (wrapN . (<+> op ":" <+> t) . prettyVar) v </> op "→" <+> b)) where
    (wrapN, wrap0) = case p of { Im -> (braces, braces) ; _ -> (parens, prec (Level 1)) }

instance Prob Int (Print Inner) where
  ex t b = bind b $ \ v b ->
    prec (Level 0) (group (pretty '∃' <+> maybe (pretty '_') prettyVar v </> group (align (op ":" <+> t)) </> group (align (op "." <+> b))))

  t1 === t2 = prec (Level 4) (group (prettyAnn t1 <+> op "≡" <+> prettyAnn t2))

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

prettyAnn :: Doc (Highlight Int) doc => doc ::: doc -> doc
prettyAnn (tm ::: ty) = tm </> group (align (op ":" <+> ty))

bind :: (Int -> Print a) -> (Maybe Int -> Print a -> Print b) -> Print b
bind b f = Print $ do
  v <- fresh
  (fvs, b') <- listen @IntSet.IntSet (runPrint (b v))
  runPrint (f (v <$ guard (v `IntSet.member` fvs)) (pure b'))

toDoc :: Print Inner -> PP.Doc (Highlight Int)
toDoc (Print m) = rainbow (runPrec (snd (run (runWriter (evalFresh 0 (getAp m))))) (Level 0))
