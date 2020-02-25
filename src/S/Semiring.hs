module S.Semiring
( Semiring(..)
, zero
) where

class Semigroup r => Semiring r where
  (><) :: r -> r -> r

  infixr 7 ><

zero :: Monoid m => m
zero = mempty
