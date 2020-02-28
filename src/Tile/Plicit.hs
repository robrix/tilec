{-# LANGUAGE DeriveFunctor #-}
module Tile.Plicit
( Plicit(..)
) where

data Plicit a
  = Im a
  | Ex a
  deriving (Eq, Functor, Ord, Show)
