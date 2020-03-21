{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Syntax
  -- * Let bindings
, Let(..)
  -- * Lambda abstraction & application
, Lam(..)
  -- * Types
, Type(..)
, (-->)
, (==>)
  -- * Existentials & equations
, Prob(..)
  -- * Modules, imports, & declarations
, Module(..)
, Import(..)
, Def(..)
  -- * Elaborator scripts
, runScript
, Script(..)
, meta
, intro
, letbind
  -- * Typing syntax
, (:::)(..)
, tm
, ty
  -- * Re-exports
, Plicit(..)
, plicit
) where

import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import Tile.Plicit

type Syntax expr = (Let expr, Lam expr, Type expr)


-- Let bindings

class Let expr where
  let' :: expr ::: expr -> (expr -> expr) -> expr


-- Lambda abstraction & application

class Lam expr where
  lam :: Plicit -> (expr -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$


-- Types

class Type expr where
  type' :: expr

  (>->) :: (Plicit, expr) -> (expr -> expr) -> expr
  infixr 6 >->

(-->) :: Type expr => expr -> expr -> expr
a --> b = (Ex, a) >-> const b

infixr 6 -->

(==>) :: Type expr => expr -> (expr -> expr) -> expr
a ==> b = (Im, a) >-> b

infixr 6 ==>


-- Existentials & equations

class Prob expr where
  ex :: expr -> (expr -> expr) -> expr
  infixr 6 `ex`

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===


-- Modules, imports, & declarations

class Module decl repr | repr -> decl where
  module' :: String -> decl -> repr


class Import repr where
  import' :: String -> repr


class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a


-- FIXME: packages


-- Elaborator scripts

runScript :: (a -> t) -> Script t a -> t
runScript k (Script r) = r k

newtype Script t a = Script ((a -> t) -> t)
  deriving (Functor)

instance Applicative (Script t) where
  pure = Script . flip ($)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Script t) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)
  {-# INLINE (>>=) #-}

meta :: Prob t => t -> Script t t
meta = Script . ex

intro :: Lam t => Plicit -> Script t t
intro = Script . lam

letbind :: Let t => t ::: t -> Script t t
letbind = Script . let'


-- Typing syntax

data a ::: b = a ::: b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable (:::) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (:::) where
  bimap = bimapDefault

instance Bitraversable (:::) where
  bitraverse f g (l ::: r) = (:::) <$> f l <*> g r

instance Show a => Show1 ((:::) a) where
  liftShowsPrec sp _ p (a ::: b) = showParen (p > 1) $ shows a . showString " ::: " . sp 2 b

infix 5 :::


tm :: a ::: b -> a
tm (a ::: _) = a

ty :: a ::: b -> b
ty (_ ::: b) = b
