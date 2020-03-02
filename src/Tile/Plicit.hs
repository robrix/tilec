{-# LANGUAGE LambdaCase #-}
module Tile.Plicit
( Plicit(..)
, plicit
) where

import Data.Ix

data Plicit
  = Im
  | Ex
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

plicit :: a -> a -> Plicit -> a
plicit im ex = \case
  Im -> im
  Ex -> ex
