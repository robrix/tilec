{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Var(..)
, Free(..)
, Let(..)
, Lam(..)
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
import Control.Monad.Trans.Reader
import Data.Functor.Const
import Data.Functor.Identity
import Tile.Plicit
import Tile.Type

class Var v expr | expr -> v where
  var :: v -> expr

deriving instance Var v t => Var v (Identity t)
deriving instance Var v t => Var v (Const t a)

instance Var v t => Var v (r -> t) where
  var = const . var

deriving instance Var v (m a) => Var v (ReaderC r m a)
deriving instance Var v (m a) => Var v (ReaderT r m a)

instance Var v t => Var v (Either e t) where
  var = pure . var


class Var v expr => Free v expr where
  free :: String -> expr

deriving instance Free v t => Free v (Identity t)
deriving instance Free v t => Free v (Const t a)

instance Free v t => Free v (r -> t) where
  free = const . free

deriving instance Free v (m a) => Free v (ReaderC r m a)
deriving instance Free v (m a) => Free v (ReaderT r m a)


class Var v expr => Let v expr where
  let' :: expr ::: expr -> (v -> expr) -> expr

deriving instance Let v t => Let v (Identity t)
deriving instance Let v t => Let v (Const t a)

instance Let v t => Let v (r -> t) where
  let' (v ::: t) b r = let' (v r ::: t r) (($ r) . b)

deriving instance Let v (m a) => Let v (ReaderC r m a)
deriving instance Let v (m a) => Let v (ReaderT r m a)


class Var v expr => Lam v expr where
  lam :: Plicit -> (v -> expr) -> expr

  ($$) :: expr -> expr -> expr
  infixl 9 $$

deriving instance Lam v t => Lam v (Identity t)
deriving instance Lam v t => Lam v (Const t a)

instance Lam v t => Lam v (r -> t) where
  lam p b r = lam p (($ r) . b)

  (f $$ a) r = f r $$ a r

deriving instance Lam v (m a) => Lam v (ReaderC r m a)
deriving instance Lam v (m a) => Lam v (ReaderT r m a)


class Var v expr => Type v expr where
  type' :: expr

  (>->) :: (Plicit, expr) -> (v -> expr) -> expr
  infixr 6 >->

deriving instance Type v t => Type v (Identity t)
deriving instance Type v t => Type v (Const t a)

instance Type v t => Type v (r -> t) where
  type' = const type'

  (t >-> b) r = fmap ($ r) t >-> ($ r) . b

deriving instance Type v (m a) => Type v (ReaderC r m a)
deriving instance Type v (m a) => Type v (ReaderT r m a)

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

deriving instance Prob v t => Prob v (Identity t)
deriving instance Prob v t => Prob v (Const t a)

instance Prob v t => Prob v (r -> t) where
  ex t b r = ex (t r) (($ r) . b)

  ((tm1 ::: ty1) === (tm2 ::: ty2)) r = (tm1 r ::: ty1 r) === (tm2 r ::: ty2 r)

deriving instance Prob v (m a) => Prob v (ReaderC r m a)
deriving instance Prob v (m a) => Prob v (ReaderT r m a)


class Err v expr | expr -> v where
  freeVariable :: v -> expr

deriving instance Err v t => Err v (Identity t)
deriving instance Err v t => Err v (Const t a)

instance Err v t => Err v (r -> t) where
  freeVariable = const . freeVariable

deriving instance Err v (m a) => Err v (ReaderC r m a)
deriving instance Err v (m a) => Err v (ReaderT r m a)


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
