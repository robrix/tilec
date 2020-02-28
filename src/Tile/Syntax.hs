{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
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
, runScript
, Script(..)
, meta
, introduce
) where

import Control.Carrier.Reader
import Control.Monad (ap)
import Control.Monad.Trans.Class
import Tile.Type

class Var a expr where
  var :: a -> expr a

instance Var a m => Var a (ReaderC r m) where
  var = ReaderC . const . var

class Var a expr => Let a expr where
  let' :: expr a -> (a -> expr a) -> expr a

instance Let a m => Let a (ReaderC r m) where
  let' v b = ReaderC (\ r -> let' (runReader r v) (runReader r . b))


class Var a expr => Lam a expr where
  lam :: (a -> expr a) -> expr a

  ($$) :: expr a -> expr a -> expr a
  infixl 9 $$

instance Lam a m => Lam a (ReaderC r m) where
  lam b = ReaderC (\ r -> lam (runReader r . b))

  f $$ a = ReaderC (\ r -> runReader r f $$ runReader r a)


class Var a expr => Type a expr where
  type' :: expr a

  (>->) :: expr a -> (a -> expr a) -> expr a
  infixr 0 >->

  (.:.) :: expr a -> expr a -> expr a
  infixl 0 .:.

instance Type a m => Type a (ReaderC r m) where
  type' = ReaderC (const type')

  t >-> b = ReaderC (\ r -> runReader r t >-> runReader r . b)

  m .:. t = ReaderC (\ r -> runReader r m .:. runReader r t)

(-->) :: Type a expr => expr a -> expr a -> expr a
a --> b = a >-> const b

infixr 0 -->


class Var a expr => Prob a expr where
  ex :: expr a -> (a -> expr a) -> expr a

  (===) :: expr a ::: expr a -> expr a ::: expr a -> expr a
  infixl 4 ===

instance Prob a m => Prob a (ReaderC r m) where
  ex t b = ReaderC (\ r -> ex (runReader r t) (runReader r . b))

  (tm1 ::: ty1) === (tm2 ::: ty2) = ReaderC (\ r -> (runReader r tm1 ::: runReader r ty1) === (runReader r tm2 ::: runReader r ty2))


class Err expr where
  err :: String -> expr a

instance Err m => Err (ReaderC r m) where
  err = ReaderC . const . err


class Def tm ty a def | def -> tm ty where
  def :: tm a -> ty a -> def a


-- FIXME: modules
-- FIXME: packages


runScript :: (a -> t v) -> Script v t a -> t v
runScript k (Script r) = r k

newtype Script v t a = Script ((a -> t v) -> t v)
  deriving (Functor)

instance Applicative (Script v t) where
  pure a = Script (\ k -> k a)
  (<*>) = ap

instance Monad (Script v t) where
  Script r >>= f = Script (\ k -> r (runScript k . f))

instance MonadTrans (Script v) where
  lift m = Script (m >>=)

meta :: Prob v t => Script v t v -> Script v t v
meta t = Script (ex (runScript var t))

introduce :: Lam v t => Script v t v
introduce = Script lam
