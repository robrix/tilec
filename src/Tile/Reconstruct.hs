-- | Type reconstruction in tagless final style.
--
-- Inserting an implicit abstraction:
--
-- @
-- id
--   : { a : Type } -> a -> a
--   = \ a . a
-- @
-- @
-- id
--   : { a : Type } -> a -> a
--   = \ {_} a . a
-- @
--
-- Inserting an implicit application:
--
-- @
-- id true
-- @
-- @
-- id {?} true
-- @
module Tile.Reconstruct
( Reconstruct(..)
) where

newtype Reconstruct t = Reconstruct { runReconstruct :: t }
