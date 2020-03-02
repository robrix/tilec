{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct t a = Reconstruct { runReconstruct :: a }

instance Var v t => Var v (Reconstruct t t) where
  var = Reconstruct . var
