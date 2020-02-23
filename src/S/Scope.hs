{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module S.Scope
( Scope(..)
, abstract
, instantiate
) where

import Control.Algebra
import Control.Monad (ap)
import Control.Monad.Trans.Class
import GHC.Generics (Generic1)

newtype Scope t a = Scope { unScope :: t (Maybe a) }
  deriving (Foldable, Functor, Generic1, Traversable)

instance HFunctor Scope where
  hmap f (Scope b) = Scope (f b)

instance Monad t => Applicative (Scope t) where
  pure = lift . pure
  (<*>) = ap

instance Monad t => Monad (Scope t) where
  Scope t >>= f = Scope (t >>= maybe (pure Nothing) (unScope . f))

instance MonadTrans Scope where
  lift = Scope . fmap Just

abstract :: (Functor t, Eq a) => a -> t a -> Scope t a
abstract a = Scope . fmap (\ a' -> if a == a' then Nothing else Just a')

instantiate :: Monad t => t a -> Scope t a -> t a
instantiate a t = unScope t >>= maybe a pure
