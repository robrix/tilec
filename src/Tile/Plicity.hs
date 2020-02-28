{-# LANGUAGE DeriveFunctor #-}
module Tile.Plicity
( Plicit(..)
) where

data Plicit a
  = Im a
  | Ex a
  deriving (Eq, Functor, Ord, Show)
