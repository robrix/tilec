module Tile.Reconstruct
( Reconstruct(..)
) where

newtype Reconstruct t a = Reconstruct { runReconstruct :: a }
