{-# LANGUAGE DeriveFunctor #-}
module Tile.Plicity
( Plicity(..)
) where

data Plicity a
  = Im a
  | Ex a
  deriving (Eq, Functor, Ord, Show)
