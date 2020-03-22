module Tile.Reconstruct
( Reconstruct(..)
) where

newtype Reconstruct t = Reconstruct { runReconstruct :: t }
