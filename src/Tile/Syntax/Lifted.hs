{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax.Lifted
( Permutable
, S.Syntax
, S.Var
, var
, S.Let
, let'
, S.Lam
, lam
, S.Type
, S.Prob
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

var :: (Applicative m, Functor i, S.Var v expr) => i v -> m (i expr)
var = pure . fmap S.var

let' :: (Applicative m, S.Let v expr, Permutable i) => (m :.: i) expr ::: (m :.: i) expr -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
let' (tm ::: ty) f = S.let' <$> ((:::) <$> tm <*> ty) <*> mapC (fmap getC) (f (C (pure id)))

lam :: (Applicative m, S.Lam v expr, Permutable i) => (m :.: i) Plicit -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
lam p f = S.lam <$> p <*> mapC (fmap getC) (f (C (pure id)))
