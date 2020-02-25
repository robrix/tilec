module S.Semiring
( Semiring(..)
) where

class Semigroup r => Semiring r where
  (><) :: r -> r -> r

  infixr 7 ><
