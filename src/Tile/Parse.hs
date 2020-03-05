module Tile.Parse
( SExpr(..)
) where

class SExpr t where
  atom :: String -> t
  list :: [t] -> t
