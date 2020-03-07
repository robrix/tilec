{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Error
( FreeVariable(..)
) where

import Data.Functor.Identity

class FreeVariable v e | e -> v where
  freeVariable :: v -> e

deriving instance FreeVariable v e => FreeVariable v (Identity e)
