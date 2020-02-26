{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Syntax.Classes
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, Prob(..)
, Err(..)
, Def(..)
) where

import S.Syntax

class Var a expr | expr -> a where
  var :: a -> expr

instance (Var a expr1, Var a expr2) => Var a (expr1 ::: expr2) where
  var a = var a ::: var a

class Var a expr => Let a expr where
  let' :: expr ::: expr -> (a -> expr) -> expr

instance (Let a expr1, Let a expr2) => Let a (expr1 ::: expr2) where
  let' ((tm1 ::: tm2) ::: (ty1 ::: ty2)) b = let' (tm1 ::: ty1) (term_ . b) ::: let' (tm2 ::: ty2) (type_ . b)

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

class Err expr where
  err :: String -> expr

class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a
