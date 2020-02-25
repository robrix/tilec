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

class Var a expr where
  var :: a -> expr a

class Var a expr => Let a expr where
  let' :: expr a ::: expr a -> (a -> expr a) -> expr a

class Var a expr => Lam a expr where
  lam :: (a -> expr a) -> expr a
  ($$) :: expr a -> expr a -> expr a

  infixl 9 $$

class Var a expr => Type a expr where
  type' :: expr a
  pi' :: expr a -> (a -> expr a) -> expr a

  infixr 0 `pi'`

(-->) :: Type a expr => expr a -> expr a -> expr a
a --> b = a `pi'` const b

infixr 0 -->

class Var a expr => Prob a expr where
  ex :: expr a -> (a -> expr a) -> expr a
  (===) :: expr a -> expr a -> expr a

  infixl 4 ===

class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a
