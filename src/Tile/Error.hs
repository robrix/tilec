{-# LANGUAGE FunctionalDependencies #-}
module Tile.Error
( FreeVariable(..)
) where

class FreeVariable v e | e -> v where
  freeVariable :: v -> e
