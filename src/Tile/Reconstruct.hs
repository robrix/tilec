{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct a t b = Reconstruct { runReconstruct :: t b }

deriving instance Var v a t => Var v a (Reconstruct a t)
deriving instance Let v a t => Let v a (Reconstruct a t)
