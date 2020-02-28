module Tile.Plicit
( Plicit(..)
) where

data Plicit
  = Im
  | Ex
  deriving (Eq, Ord, Show)
