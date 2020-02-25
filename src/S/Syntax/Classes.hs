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

class Var a expr | expr -> a where
  var :: a -> expr

class Var a expr => Let a expr where
  let' :: expr ::: expr -> (a -> expr) -> expr

class Var a expr => Lam a expr where
  lam :: (a -> expr) -> expr
  ($$) :: expr -> expr -> expr

  infixl 9 $$

class Var a expr => Type a expr where
  type' :: expr
  pi' :: expr -> (a -> expr) -> expr

  infixr 0 `pi'`

(-->) :: Type a expr => expr -> expr -> expr
a --> b = a `pi'` const b

infixr 0 -->

class Var a expr => Prob a expr where
  ex :: expr -> (a -> expr) -> expr
  (===) :: expr -> expr -> expr

  infixl 4 ===

class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a
