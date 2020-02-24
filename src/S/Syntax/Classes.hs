{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax.Classes
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, Prob(..)
, Def(..)
) where

import S.Syntax

class Var expr a where
  var :: a -> expr a

class Var expr a => Let expr a where
  let' :: expr a ::: expr a -> (a -> expr a) -> expr a

class Var expr a => Lam expr a where
  lam :: (a -> expr a) -> expr a
  ($$) :: expr a -> expr a -> expr a

  infixl 9 $$

class Var expr a => Type expr a where
  type' :: expr a
  pi' :: expr a -> (a -> expr a) -> expr a

  infixr 0 `pi'`

(-->) :: Type expr a => expr a -> expr a -> expr a
a --> b = a `pi'` const b

infixr 0 -->

class Var expr a => Prob expr a where
  ex :: expr a -> (a -> expr a) -> expr a
  (===) :: expr a -> expr a -> expr a

  infixl 4 ===

class Def expr a def | def -> expr where
  def :: expr a ::: expr a -> def a
