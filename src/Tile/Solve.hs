module Tile.Solve
( Solve(..)
) where

newtype Solve v t = Solve { runSolve :: t }
