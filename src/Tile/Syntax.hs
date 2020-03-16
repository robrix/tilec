{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Control.Monad.Trans.Reader hiding (runReader)
import Tile.Plicit
import Tile.Type

class Var v a expr | expr -> v a where
  var :: v -> expr a

instance Var v a m => Var v a (ReaderC r m) where
  var = ReaderC . const . var

instance Var v a m => Var v a (ReaderT r m) where
  var = ReaderT . const . var


class Var v a expr => Free v a expr where
  free :: String -> expr a

instance Free v a m => Free v a (ReaderC r m) where
  free = ReaderC . const . free

instance Free v a m => Free v a (ReaderT r m) where
  free = ReaderT . const . free


class Var v a expr => Let v a expr where
  let' :: expr a ::: expr a -> (v -> expr a) -> expr a

instance Let v a m => Let v a (ReaderC r m) where
  let' (v ::: t) b = ReaderC $ \ r -> let' (runReader r v ::: runReader r t) (runReader r . b)

instance Let v a m => Let v a (ReaderT r m) where
  let' (v ::: t) b = ReaderT $ \ r -> let' (runReaderT v r ::: runReaderT t r) ((`runReaderT` r) . b)


class Var v a expr => Lam v a expr where
  lam :: Plicit -> (v -> expr a) -> expr a

  ($$) :: expr a -> expr a -> expr a
  infixl 9 $$

instance Lam v a m => Lam v a (ReaderC r m) where
  lam p b = ReaderC $ \ r -> lam p (runReader r . b)

  f $$ a = ReaderC $ \ r -> runReader r f $$ runReader r a

instance Lam v a m => Lam v a (ReaderT r m) where
  lam p b = ReaderT $ \ r -> lam p ((`runReaderT` r) . b)

  f $$ a = ReaderT $ \ r -> runReaderT f r $$ runReaderT a r


class Var v a expr => Type v a expr where
  type' :: expr a

  (>->) :: (Plicit, expr a) -> (v -> expr a) -> expr a
  infixr 6 >->

instance Type v a m => Type v a (ReaderC r m) where
  type' = ReaderC (const type')

  t >-> b = ReaderC $ \ r -> fmap (runReader r) t >-> runReader r . b

instance Type v a m => Type v a (ReaderT r m) where
  type' = ReaderT (const type')

  t >-> b = ReaderT $ \ r -> fmap (`runReaderT` r) t >-> (`runReaderT` r) . b

(-->) :: Type v a expr => expr a -> expr a -> expr a
a --> b = (Ex, a) >-> const b

infixr 6 -->

(==>) :: Type v a expr => expr a -> (v -> expr a) -> expr a
a ==> b = (Im, a) >-> b

infixr 6 ==>


class Var v a expr => Prob v a expr where
  ex :: expr a -> (v -> expr a) -> expr a
  infixr 6 `ex`

  (===) :: expr a ::: expr a -> expr a ::: expr a -> expr a
  infixl 4 ===

instance Prob v a m => Prob v a (ReaderC r m) where
  ex t b = ReaderC $ \ r -> ex (runReader r t) (runReader r . b)

  (tm1 ::: ty1) === (tm2 ::: ty2) = ReaderC $ \ r -> (runReader r tm1 ::: runReader r ty1) === (runReader r tm2 ::: runReader r ty2)

instance Prob v a m => Prob v a (ReaderT r m) where
  ex t b = ReaderT $ \ r -> ex (runReaderT t r) ((`runReaderT` r) . b)

  (tm1 ::: ty1) === (tm2 ::: ty2) = ReaderT $ \ r -> (runReaderT tm1 r ::: runReaderT ty1 r) === (runReaderT tm2 r ::: runReaderT ty2 r)




class Err e a expr | expr -> e a where
  err :: e -> expr a

instance Err e a m => Err e a (ReaderC r m) where
  err = ReaderC . const . err

instance Err e a m => Err e a (ReaderT r m) where
  err = ReaderT . const . err



class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a


-- FIXME: modules
-- FIXME: packages


runScript :: (b -> m a) -> Script a m b -> m a
runScript k (Script r) = r k

newtype Script a m b = Script ((b -> m a) -> m a)
  deriving (Functor)

instance Applicative (Script a m) where
  pure = Script . flip ($)
  (<*>) = ap

instance Monad (Script a m) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)

meta :: Prob v a m => m a -> Script a m v
meta = Script . ex

intro :: Lam v a m => Plicit -> Script a m v
intro = Script . lam

letbind :: Let v a m => m a ::: m a -> Script a m v
letbind = Script . let'
