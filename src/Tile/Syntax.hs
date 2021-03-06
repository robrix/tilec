{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Syntax
  -- * Let bindings
, Let(..)
  -- * Lambda abstraction & application
, Lam(..)
, Lams(..)
  -- * Types
, Type(..)
, (-->)
, Types(..)
  -- * Existentials & equations
, Prob(..)
  -- * Modules, imports, & declarations
, Module(..)
, Import(..)
, Export(..)
, Def(..)
  -- * Typing syntax
, (:::)(..)
  -- * Definition syntax
, (:=)(..)
  -- * Polyvariadics base case
, Return(..)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes

type Syntax expr = (Let expr, Lam expr, Type expr)


-- Let bindings

class Let expr where
  let' :: expr ::: expr -> (expr -> expr) -> expr


-- Lambda abstraction & application

class Lam expr where
  lam :: (expr -> expr) -> expr
  ilam :: (expr -> expr) -> expr

  ($$) :: expr -> expr -> expr
  ($$?) :: expr -> expr -> expr

  infixl 9 $$, $$?


-- | Variadic lambda construction.
class Lams expr t | t -> expr where
  lams  :: t -> expr
  ilams :: t -> expr

instance Lams expr (Return expr) where
  lams  = getReturn
  ilams = getReturn

instance (Lam expr, Lams expr t) => Lams expr (expr -> t) where
  lams  f = lam  (lams  . f)
  ilams f = ilam (ilams . f)


-- Types

class Type expr where
  type' :: expr

  (->>) :: expr -> (expr -> expr) -> expr
  (=>>) :: expr -> (expr -> expr) -> expr

  infixr 6 ->>, =>>

(-->) :: Type expr => expr -> expr -> expr
a --> b = a ->> const b

infixr 6 -->


-- | Variadic type construction.
class Types expr t | t -> expr where
  (*=>>) :: expr -> t -> expr
  infixr 6 *=>>

instance Types expr (Return expr) where
  (*=>>) = const getReturn

instance (Type expr, Types expr t) => Types expr (expr -> t) where
  ty *=>> f = ty =>> (ty *=>>) . f


-- Existentials & equations

class Prob expr where
  ex :: expr -> (expr -> expr) -> expr
  infixr 6 `ex`

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===


-- Modules, imports, & declarations

class Module decl repr | repr -> decl where
  module' :: String -> decl a -> repr a


class Import repr where
  import' :: String -> repr


class Export repr where
  export :: a -> repr a


class Def expr def | def -> expr where
  def :: String ::: expr := expr -> (expr -> def expr) -> def expr


-- FIXME: packages


-- Typing syntax

data a ::: b = (:::) { tm :: a, ty :: b }
  deriving (Eq, Foldable, Functor, Ord, Traversable)

infix 5 :::

instance Bifoldable (:::) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (:::) where
  bimap = bimapDefault

instance Bitraversable (:::) where
  bitraverse f g (l ::: r) = (:::) <$> f l <*> g r

instance Show2 (:::) where
  liftShowsPrec2 sp1 _ sp2 _ p (a ::: b) = showParen (p > 5) $ sp1 5 a . showString " ::: " . sp2 6 b

instance Show a => Show1 ((:::) a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show b) => Show (a ::: b) where
  showsPrec = showsPrec1


-- Definition syntax

data a := b = a := b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 4 :=

instance Bifoldable (:=) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (:=) where
  bimap = bimapDefault

instance Bitraversable (:=) where
  bitraverse f g (a := b) = (:=) <$> f a <*> g b


-- Polyvariadics base case

newtype Return a = Return { getReturn :: a }
