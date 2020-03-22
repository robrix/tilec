-- | Type reconstruction in tagless final style.
module Tile.Reconstruct
( Reconstruct(..)
) where

newtype Reconstruct t = Reconstruct { runReconstruct :: t }
