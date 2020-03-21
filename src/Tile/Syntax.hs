{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Syntax
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, (==>)
, Prob(..)
, Module(..)
, Def(..)
  -- * Elaborator scripts
, runScript
, Script(..)
, meta
, intro
, letbind
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import Control.Monad (ap)
import Tile.Plicit
import Tile.Type

type Syntax expr = (Let expr, Lam expr, Type expr)

class Let expr where
  let' :: expr ::: expr -> (expr -> expr) -> expr


class Lam expr where
  lam :: Plicit -> (expr -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$


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


class Prob expr where
  ex :: expr -> (expr -> expr) -> expr
  infixr 6 `ex`

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===


class Module decl repr | repr -> decl where
  module' :: String -> decl -> repr


class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a


-- FIXME: modules
-- FIXME: packages


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
