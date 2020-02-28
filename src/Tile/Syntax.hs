{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Data.Functor.Identity
import Tile.Type

class Var v expr | expr -> v where
  var :: v -> expr

deriving instance Var v t => Var v (Identity t)

instance Var v t => Var v (r -> t) where
  var = const . var

instance Var v (m a) => Var v (ReaderC r m a) where
  var = ReaderC . const . var


class Var v expr => Let v expr where
  let' :: expr -> (v -> expr) -> expr

deriving instance Let v t => Let v (Identity t)

instance Let v (m a) => Let v (ReaderC r m a) where
  let' v b = ReaderC (\ r -> let' (runReader r v) (runReader r . b))


class Var v expr => Lam v expr where
  lam :: (v -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$

deriving instance Lam v t => Lam v (Identity t)

instance Lam v (m a) => Lam v (ReaderC r m a) where
  lam b = ReaderC (\ r -> lam (runReader r . b))

  f $$ a = ReaderC (\ r -> runReader r f $$ runReader r a)


class Var v expr => Type v expr where
  type' :: expr

  (>->) :: expr -> (v -> expr) -> expr
  infixr 0 >->

  (.:.) :: expr -> expr -> expr
  infixl 0 .:.

deriving instance Type v t => Type v (Identity t)

instance Type v (m a) => Type v (ReaderC r m a) where
  type' = ReaderC (const type')

  t >-> b = ReaderC (\ r -> runReader r t >-> runReader r . b)

  m .:. t = ReaderC (\ r -> runReader r m .:. runReader r t)

(-->) :: Type a expr => expr -> expr -> expr
a --> b = a >-> const b

infixr 0 -->


class Var v expr => Prob v expr where
  ex :: expr -> (v -> expr) -> expr

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===

deriving instance Prob v t => Prob v (Identity t)

instance Prob v (m a) => Prob v (ReaderC r m a) where
  ex t b = ReaderC (\ r -> ex (runReader r t) (runReader r . b))

  (tm1 ::: ty1) === (tm2 ::: ty2) = ReaderC (\ r -> (runReader r tm1 ::: runReader r ty1) === (runReader r tm2 ::: runReader r ty2))


class Err expr where
  err :: String -> expr

deriving instance Err t => Err (Identity t)

instance Err (m a) => Err (ReaderC r m a) where
  err = ReaderC . const . err


class Def tm ty a def | def -> tm ty where
  def :: tm a -> ty a -> def a


-- FIXME: modules
-- FIXME: packages


runScript :: (a -> t) -> Script t a -> t
runScript k (Script r) = r k

newtype Script t a = Script ((a -> t) -> t)
  deriving (Functor)

instance Applicative (Script t) where
  pure a = Script (\ k -> k a)
  (<*>) = ap

instance Monad (Script t) where
  Script r >>= f = Script (\ k -> r (runScript k . f))

meta :: Prob v t => Script t v -> Script t v
meta = Script . ex . runScript var

introduce :: Lam v t => Script t v
introduce = Script lam
