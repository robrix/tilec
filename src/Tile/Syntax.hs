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

deriving instance Var v (m a) => Var v (ReaderC r m a)


class Var v expr => Let v expr where
  let' :: expr -> (v -> expr) -> expr

deriving instance Let v t => Let v (Identity t)

instance Let v t => Let v (r -> t) where
  let' v b r = let' (v r) (($ r) . b)

deriving instance Let v (m a) => Let v (ReaderC r m a)


class Var v expr => Lam v expr where
  lam :: (v -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$

deriving instance Lam v t => Lam v (Identity t)

instance Lam v t => Lam v (r -> t) where
  lam b r = lam (($ r) . b)

  (f $$ a) r = f r $$ a r

deriving instance Lam v (m a) => Lam v (ReaderC r m a)


class Var v expr => Type v expr where
  type' :: expr

  (>->) :: expr -> (v -> expr) -> expr
  infixr 0 >->

  (.:.) :: expr -> expr -> expr
  infixl 0 .:.

deriving instance Type v t => Type v (Identity t)

instance Type v t => Type v (r -> t) where
  type' = const type'

  (t >-> b) r = t r >-> ($ r) . b

  (m .:. t) r = m r .:. t r

deriving instance Type v (m a) => Type v (ReaderC r m a)

(-->) :: Type a expr => expr -> expr -> expr
a --> b = a >-> const b

infixr 0 -->


class Var v expr => Prob v expr where
  ex :: expr -> (v -> expr) -> expr

  (===) :: expr ::: expr -> expr ::: expr -> expr
  infixl 4 ===

deriving instance Prob v t => Prob v (Identity t)

instance Prob v t => Prob v (r -> t) where
  ex t b r = ex (t r) (($ r) . b)

  ((tm1 ::: ty1) === (tm2 ::: ty2)) r = (tm1 r ::: ty1 r) === (tm2 r ::: ty2 r)

deriving instance Prob v (m a) => Prob v (ReaderC r m a)


class Err expr where
  err :: String -> expr

deriving instance Err t => Err (Identity t)

instance Err t => Err (r -> t) where
  err = const . err

deriving instance Err (m a) => Err (ReaderC r m a)


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
