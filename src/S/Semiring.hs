module S.Semiring
( Semiring(..)
, zero
, Unital(..)
) where

class Semigroup r => Semiring r where
  (><) :: r -> r -> r

  infixr 7 ><

zero :: Monoid m => m
zero = mempty

class Semiring r => Unital r where
  one :: r
