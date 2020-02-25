module S.Semiring
( Semiring(..)
, zero
, Unital(..)
) where

import Control.Applicative (liftA2)

class Semigroup r => Semiring r where
  (><) :: r -> r -> r

  infixr 7 ><

zero :: Monoid m => m
zero = mempty

class Semiring r => Unital r where
  one :: r


instance Semiring () where
  _ >< _ = ()

instance Unital () where
  one = ()


instance Semiring b => Semiring (a -> b) where
  (><) = liftA2 (><)

instance Unital b => Unital (a -> b) where
  one = pure one


instance Semiring a => Semiring (Maybe a) where
  (><) = liftA2 (><)

instance Unital a => Unital (Maybe a) where
  one = pure one
