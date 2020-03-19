module Tile.Permutable
( Lam(..)
) where

class Lam repr where
  lamPure :: (repr a -> repr b) -> repr (a -> b)
  appPure :: repr (a -> b) -> (repr a -> repr b)
