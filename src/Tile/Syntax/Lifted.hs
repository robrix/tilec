{-# LANGUAGE ConstraintKinds #-}
module Tile.Syntax.Lifted
( Permutable
, S.Var
, var
) where

import           Data.Distributive
import qualified Tile.Syntax as S

type Permutable f = (Applicative f, Distributive f)

var :: (Applicative m, Functor i, S.Var v expr) => i v -> m (i expr)
var = pure . fmap S.var
