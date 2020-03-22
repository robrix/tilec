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
  -- * Types
, Type(..)
, (-->)
, (==>)
  -- * Existentials & equations
, Prob(..)
  -- * Modules, imports, & declarations
, Module(..)
, Import(..)
, Export(..)
, Def(..)
  -- * Elaborator scripts
, runScript
, evalScript
, shift
, Script(..)
, (.:)
, meta
, intro
, letbind
  -- * Typing syntax
, (:::)(..)
  -- * Definition syntax
, (:=)(..)
  -- * {Im,ex}plicitness
, Plicit(..)
, plicit
) where

import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import Data.Ix

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
  module' :: String -> decl a -> repr a


class Import repr where
  import' :: String -> repr


class Export repr where
  export :: a -> repr a


class Def expr def | def -> expr where
  def :: String ::: expr := expr -> (expr -> def expr) -> def expr


-- FIXME: packages


-- Elaborator scripts

runScript :: (a -> t) -> Script t a -> t
runScript = flip getScript

evalScript :: Script t t -> t
evalScript = runScript id

shift :: ((a -> t) -> Script t t) -> Script t a
shift f = Script (evalScript . f)

newtype Script t a = Script { getScript :: (a -> t) -> t }
  deriving (Functor)

instance Applicative (Script t) where
  pure = Script . flip ($)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Script t) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)
  {-# INLINE (>>=) #-}

(.:) :: Def t def => String -> t := t -> Script (def t) t
name .: (ty := tm) = Script $ def (name ::: ty := tm)

infix 3 .:

meta :: Prob t => t -> Script t t
meta = Script . ex

intro :: Lam t => Plicit -> Script t t
intro = Script . lam

letbind :: Let t => t ::: t -> Script t t
letbind = Script . let'


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


-- {Im,ex}plicitness

data Plicit
  = Im
  | Ex
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

plicit :: a -> a -> Plicit -> a
plicit im ex = \case
  Im -> im
  Ex -> ex
