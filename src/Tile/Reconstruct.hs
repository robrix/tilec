{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

newtype Reconstruct a m b = Reconstruct { runReconstruct :: m b }
  deriving (Applicative, Functor, Monad)
