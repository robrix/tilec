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
  lam _ = Reconstruct id
  ilam _ = Reconstruct id

  _ $$ _ = Reconstruct id
  _ $$? _ = Reconstruct id
