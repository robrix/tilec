{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct t a = Reconstruct { runReconstruct :: a }

instance Var v t => Var v (Reconstruct t t) where
  var = Reconstruct . var

deriving instance Let v t => Let v (Reconstruct t t)
