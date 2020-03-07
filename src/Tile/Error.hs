{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Error
( FreeVariable(..)
) where

import Control.Carrier.Reader
import Data.Functor.Const
import Data.Functor.Identity

class FreeVariable v e | e -> v where
  freeVariable :: v -> e

deriving instance FreeVariable v e => FreeVariable v (Identity e)
deriving instance FreeVariable v e => FreeVariable v (Const e a)

instance FreeVariable v e => FreeVariable v (r -> e) where
  freeVariable = const . freeVariable

deriving instance FreeVariable v (m a) => FreeVariable v (ReaderC r m a)
