{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
, Extends(..)
, trace
, CPSA(..)
) where

import Control.Applicative (liftA2)
import Data.Coerce
import Data.Distributive

newtype (f :.: g) a = C { getC :: f (g a) }
  deriving (Functor)

infixr 7 :.:

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  {-# INLINE pure #-}

  C f <*> C a = C (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

  liftA2 f (C a) (C b) = C (liftA2 (liftA2 f) a b)
  {-# INLINE liftA2 #-}

  C a *> C b = C (liftA2 (*>) a b)
  {-# INLINE (*>) #-}

  C a <* C b = C (liftA2 (<*) a b)
  {-# INLINE (<*) #-}

instance (Distributive f, Distributive g) => Distributive (f :.: g) where
  distribute = C . fmap distribute . collect coerce
  {-# INLINE distribute #-}

  collect f = C . fmap distribute . collect (coerce f)
  {-# INLINE collect #-}

liftC :: (Functor m, Applicative i) => m a -> (m :.: i) a
liftC = C . fmap pure

mapC :: (f (g a) -> f' (g' a')) -> ((f :.: g) a -> (f' :.: g') a')
mapC = coerce

assocL :: Functor f => (f :.: (g :.: h)) a -> ((f :.: g) :.: h) a
assocL = C . mapC (fmap getC)

assocR :: Functor f => ((f :.: g) :.: h) a -> (f :.: (g :.: h)) a
assocR = mapC (fmap C) . getC


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
lam f = lamPure <$> C (getC <$> getC (f (C (pure id))))


var :: Applicative m => i (repr a) -> (m :.: i) (repr a)
var = C . pure

vr :: forall m i j repr a . (Applicative m, Extends (m :.: i) (m :.: j)) => i (repr a) -> (m :.: j) (repr a)
vr = weakens . var @m


weaken :: (Applicative m, Applicative i, Applicative j) => (m :.: i) (repr a) -> (m :.: i :.: j) (repr a)
weaken = weakens

class (Applicative m, Applicative n) => Extends m n where
  weakens :: m a -> n a

instance (Applicative f, Extends g1 g2) => Extends (f :.: g1) (f :.: g2) where
  weakens = C . fmap weakens . getC

instance (Applicative f, Applicative g) => Extends f (f :.: g) where
  weakens = liftC

instance Applicative f => Extends f f where
  weakens = id


trace :: Applicative i => String -> (IO :.: i) ()
trace = liftC . putStrLn


newtype CPSA w m a = CPSA
  { runCPSA
    :: forall hw
    .  Permutable hw
    => (forall h . Permutable h => ((m :.: hw) :.: h) a -> ((m :.: hw) :.: h) w)
    -> (m :.: hw) w
  }

instance Functor m => Functor (CPSA w m) where
  fmap f (CPSA run) = CPSA $ \ k -> run (k . fmap f)
  {-# INLINE fmap #-}
