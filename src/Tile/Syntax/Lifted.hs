{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax.Lifted
( Permutable
, S.Syntax
  -- * Var
, S.Var
, var
  -- * Let
, S.Let
, let'
  -- * Lam
, S.Lam
, lam
  -- * Type
, S.Type
  -- * Prob
, S.Prob
, ex
  -- * Modules
, S.Def
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import           Data.Distributive
import           Tile.Functor.Compose
import           Tile.Plicit
import qualified Tile.Syntax as S
import           Tile.Type

type Permutable f = (Applicative f, Distributive f)

-- Var

var :: (Applicative m, Functor i, S.Var v expr) => i v -> m (i expr)
var = pure . fmap S.var


-- Let

let' :: (Applicative m, S.Let v expr, Permutable i) => (m :.: i) expr ::: (m :.: i) expr -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
let' (tm ::: ty) f = S.let' <$> ((:::) <$> tm <*> ty) <*> mapC (fmap getC) (f (C (pure id)))


-- Lam

lam :: (Applicative m, S.Lam v expr, Permutable i) => (m :.: i) Plicit -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
lam p f = S.lam <$> p <*> mapC (fmap getC) (f (C (pure id)))


-- Prob

ex :: (Applicative m, S.Prob v expr, Permutable i) => (m :.: i) expr -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
ex t f = S.ex <$> t <*> mapC (fmap getC) (f (C (pure id)))
