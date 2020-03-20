{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct a m b = Reconstruct { runReconstruct :: m b }
  deriving (Applicative, Functor, Monad, Var v, Lam v, Let v, Type v, Prob v)
