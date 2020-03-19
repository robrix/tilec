{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Permutable
( (:.:)(..)
, liftC
, mapC
, assocL
, assocR
, assocLR
, assocRL
, Permutable
, Boolean(..)
, false
, true
, (|||)
, (&&&)
, not'
, Lam(..)
, ($$)
, lam
, var
, vr
, weaken
, strengthen
, Extends(..)
, trace
, CPSA(..)
, runCPSA
, runCCPSA
, throw
, reset
, resetC
, liftCPSA
, liftCCPSA
) where

import Control.Applicative (liftA2)
import Data.Distributive
import Data.Functor.Identity
import Tile.Functor.Compose

type Permutable f = (Applicative f, Distributive f)


class Boolean repr where
  liftBool :: Bool -> repr Bool
  if' :: repr Bool -> repr a -> repr a -> repr a

false, true :: Boolean repr => repr Bool
false = liftBool False
true  = liftBool True

(|||), (&&&) :: Boolean repr => repr Bool -> repr Bool -> repr Bool
a ||| b = if' a true b
a &&& b = if' a b false

infixr 2 |||
infixr 3 &&&

not' :: Boolean repr => repr Bool -> repr Bool
not' x = if' x false true


class Lam repr where
  lamPure :: (repr a -> repr b) -> repr (a -> b)
  appPure :: repr (a -> b) -> (repr a -> repr b)

($$) :: (Lam repr, Applicative m) => m (repr (a -> b)) -> (m (repr a) -> m (repr b))
($$) = liftA2 appPure

infixl 9 $$

lam :: (Applicative m, Lam repr, Permutable i) => (forall j . Permutable j => (i :.: j) (repr a) -> (m :.: i :.: j) (repr b)) -> (m :.: i) (repr (a -> b))
lam f = lamPure <$> mapC (fmap getC) (f (C (pure id)))


var :: Applicative m => i (repr a) -> (m :.: i) (repr a)
var = C . pure

vr :: forall m i j repr a . (Applicative m, Extends (m :.: i) (m :.: j)) => i (repr a) -> (m :.: j) (repr a)
vr = weakens . var @m


weaken :: (Applicative m, Applicative i, Applicative j) => (m :.: i) (repr a) -> (m :.: i :.: j) (repr a)
weaken = weakens

strengthen :: Functor m => (m :.: Identity) a -> m a
strengthen = fmap runIdentity . getC

class (Applicative m, Applicative n) => Extends m n where
  weakens :: m a -> n a

instance (Applicative f, Extends g1 g2) => Extends (f :.: g1) (f :.: g2) where
  weakens = mapC (fmap weakens)

instance (Applicative f, Applicative g) => Extends f (f :.: g) where
  weakens = liftC

instance Applicative f => Extends f f where
  weakens = id


trace :: Applicative i => String -> (IO :.: i) ()
trace = liftC . putStrLn


newtype CPSA w m a = CPSA
  { getCPSA
    :: forall hw
    .  Permutable hw
    => (forall h . Permutable h => ((m :.: hw) :.: h) a -> ((m :.: hw) :.: h) w)
    -> (m :.: hw) w
  }

instance Functor m => Functor (CPSA w m) where
  fmap f (CPSA run) = CPSA $ \ k -> run (k . fmap f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (CPSA w m) where
  pure a = CPSA $ \ k -> strengthen (k (pure a))
  {-# INLINE pure #-}

  CPSA f <*> CPSA a = CPSA $ \ k -> f (\ f' -> assocL (a (assocLR . k . (assocR (liftC f') <*>) . assocRL)))
  {-# INLINE (<*>) #-}

runCPSA :: Functor m => CPSA a m a -> m a
runCPSA = strengthen . (`getCPSA` id)

runCCPSA :: Functor m => (CPSA (i a) m :.: i) a -> (m :.: i) a
runCCPSA = mapC runCPSA

throw
  :: (Applicative m, Applicative hw)
  => (forall h . Permutable h => ((m :.: hw) :.: h) a -> ((m :.: hw) :.: h) w)
  -> m a
  -> (m :.: hw) w
throw k = strengthen . k . liftC . liftC

reset :: Applicative m => CPSA a m a -> CPSA w m a
reset m = CPSA $ \k -> throw k $ runCPSA m

resetC :: Applicative m => (CPSA (i a) m :.: i) a -> (CPSA w m :.: i) a
resetC = mapC reset

liftCPSA :: Functor m => m a -> CPSA w m a
liftCPSA m = CPSA $ \ k -> strengthen (k (liftC (liftC m)))

liftCCPSA :: Functor m => (m :.: i) a -> (CPSA w m :.: i) a
liftCCPSA = mapC liftCPSA
