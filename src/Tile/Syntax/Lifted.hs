{-# LANGUAGE ConstraintKinds #-}
module Tile.Syntax.Lifted
( Permutable
, S.Syntax
, S.Var
, var
, S.Let
, S.Lam
, S.Type
, S.Prob
, S.Def
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import           Data.Distributive
import           Tile.Plicit
import qualified Tile.Syntax as S
import           Tile.Type

type Permutable f = (Applicative f, Distributive f)

var :: (Applicative m, Functor i, S.Var v expr) => i v -> m (i expr)
var = pure . fmap S.var
