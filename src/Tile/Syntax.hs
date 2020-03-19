{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Permutable
, Var(..)
, varA
, Let(..)
, Lam(..)
, lamA
, Type(..)
, (-->)
, (==>)
, Prob(..)
, Err(..)
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

import Control.Carrier.Reader
import Control.Monad (ap)
import Data.Distributive
import Tile.Functor.Compose
import Tile.Plicit
import Tile.Type

type Permutable f = (Applicative f, Distributive f)

class Var v expr | expr -> v where
  var :: v -> expr

instance Var v (m a) => Var v (ReaderC r m a) where
  var = ReaderC . const . var

varA :: (Applicative m, Functor i, Var v expr) => i v -> m (i expr)
varA = pure . fmap var


class Var v expr => Let v expr where
  let' :: expr ::: expr -> (v -> expr) -> expr

instance Let v (m a) => Let v (ReaderC r m a) where
  let' (v ::: t) b = ReaderC $ \ r -> let' (runReader r v ::: runReader r t) (runReader r . b)


class Var v expr => Lam v expr where
  lam :: Plicit -> (v -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$

instance Lam v (m a) => Lam v (ReaderC r m a) where
  lam p b = ReaderC $ \ r -> lam p (runReader r . b)

  f $$ a = ReaderC $ \ r -> runReader r f $$ runReader r a

lamA :: (Applicative m, Lam v expr, Permutable i) => Plicit -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) (expr)
lamA p f = lam p <$> mapC (fmap getC) (f (C (pure id)))


class Var v expr => Type v expr where
  type' :: expr

  (>->) :: (Plicit, expr) -> (v -> expr) -> expr
  infixr 6 >->

instance Type v (m a) => Type v (ReaderC r m a) where
  type' = ReaderC (const type')

  t >-> b = ReaderC $ \ r -> fmap (runReader r) t >-> runReader r . b

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

instance Prob v (m a) => Prob v (ReaderC r m a) where
  ex t b = ReaderC $ \ r -> ex (runReader r t) (runReader r . b)

  (tm1 ::: ty1) === (tm2 ::: ty2) = ReaderC $ \ r -> (runReader r tm1 ::: runReader r ty1) === (runReader r tm2 ::: runReader r ty2)


class Err e expr | expr -> e where
  err :: e -> expr

instance Err e (m a) => Err e (ReaderC r m a) where
  err = ReaderC . const . err


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
  (<*>) = ap

instance Monad (Script t) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)

meta :: Prob v t => t -> Script t v
meta = Script . ex

intro :: Lam v t => Plicit -> Script t v
intro = Script . lam

letbind :: Let v t => t ::: t -> Script t v
letbind = Script . let'
