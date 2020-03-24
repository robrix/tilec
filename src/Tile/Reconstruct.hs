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
-- (∃ ?t : Type . ∃ ?x : ?t . id {?x}) true
-- @
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct t = Reconstruct { runReconstruct :: t -> t }

instance Lam (Reconstruct t) where
  lam _ _ = Reconstruct id

  app _ _ = Reconstruct id
