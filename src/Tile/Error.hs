module Tile.Error
( Error(..)
) where

newtype Error v
  = FreeVariable v
  deriving (Eq, Ord, Show)
