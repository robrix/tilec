{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, Prob(..)
, Err(..)
, Def(..)
) where

import Tile.Type

class Var a expr | expr -> a where
  var :: a -> expr a


class Var a expr => Let a expr where
  let' :: expr a -> (a -> expr a) -> expr a


class Var a expr => Lam a expr where
  lam :: (a -> expr a) -> expr a

  ($$) :: expr a -> expr a -> expr a
  infixl 9 $$


class Var a expr => Type a expr where
  type' :: expr a

  (>->) :: expr a -> (a -> expr a) -> expr a
  infixr 0 >->

  (.:.) :: expr a -> expr a -> expr a
  infixl 0 .:.

(-->) :: Type a expr => expr a -> expr a -> expr a
a --> b = a >-> const b

infixr 0 -->


class Var a expr => Prob a expr where
  ex :: expr a -> (a -> expr a) -> expr a

  (===) :: expr a ::: expr a -> expr a ::: expr a -> expr a
  infixl 4 ===


class Err expr where
  err :: String -> expr a


class Def tm ty a def | def -> tm ty where
  def :: tm a -> ty a -> def a


-- FIXME: modules
-- FIXME: packages
