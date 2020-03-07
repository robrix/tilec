{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Error
( FreeVariable(..)
, Error(..)
) where

import Control.Carrier.Reader
import Control.Monad.Trans.Reader
import Data.Functor.Const
import Data.Functor.Identity

class FreeVariable v e | e -> v where
  freeVariable :: v -> e

deriving instance FreeVariable v e => FreeVariable v (Identity e)
deriving instance FreeVariable v e => FreeVariable v (Const e a)

instance FreeVariable v e => FreeVariable v (r -> e) where
  freeVariable = const . freeVariable

deriving instance FreeVariable v (m a) => FreeVariable v (ReaderC r m a)
deriving instance FreeVariable v (m a) => FreeVariable v (ReaderT r m a)


newtype Error v
  = FreeVariable v
  deriving (Eq, Ord, Show)
