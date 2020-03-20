{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Syntax
, Var(..)
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, (==>)
, Prob(..)
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

type Syntax v expr = (Let v expr, Lam v expr, Type v expr)

class Var v expr | expr -> v where
  var :: v -> expr


class Var v expr => Let v expr where
  let' :: expr ::: expr -> (v -> expr) -> expr


class Var v expr => Lam v expr where
  lam :: Plicit -> (v -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$


class Var v expr => Type v expr where
  type' :: expr

  (>->) :: (Plicit, expr) -> (v -> expr) -> expr
  infixr 6 >->

(-->) :: Type v expr => expr -> expr -> expr
a --> b = (Ex, a) >-> const b

infixr 6 -->

(==>) :: Type v expr => expr -> (v -> expr) -> expr
a ==> b = (Im, a) >-> b

infixr 6 ==>


class Var v expr => Prob v expr where
  ex :: expr -> (v -> expr) -> expr
  infixr 6 `ex`

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===


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

meta :: Prob v t => t -> Script t v
meta = Script . ex

intro :: Lam v t => Plicit -> Script t v
intro = Script . lam

letbind :: Let v t => t ::: t -> Script t v
letbind = Script . let'
