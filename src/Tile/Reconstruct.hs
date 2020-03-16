{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Reconstruct
( Reconstruct(..)
) where

import Tile.Syntax

newtype Reconstruct a t b = Reconstruct { runReconstruct :: t b }
  deriving (Applicative, Functor, Monad, Var v a, Lam v a, Let v a, Type v a, Prob v a, Err e a)
