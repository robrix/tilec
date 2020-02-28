{-# LANGUAGE DeriveTraversable #-}
module Tile.Plicit
( Plicit(..)
) where

data Plicit a
  = Im a
  | Ex a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
