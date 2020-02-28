module Tile.Plicit
( Plicit(..)
) where

import Data.Ix

data Plicit
  = Im
  | Ex
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)
