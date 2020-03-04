{-# LANGUAGE DeriveTraversable #-}
module Tile.Error
( Error(..)
) where

newtype Error v
  = FreeVariable v
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
