{-# LANGUAGE ConstraintKinds #-}
module Tile.Syntax.Lifted
( Permutable
) where

import Data.Distributive

type Permutable f = (Applicative f, Distributive f)
