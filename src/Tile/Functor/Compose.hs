{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Functor.Compose
( (:.:)(..)
, liftC
, mapC
, assocL
, assocR
, assocLR
, assocRL
, weaken
, strengthen
, Permutable
, Extends(..)
, Tr(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Coerce (coerce)
import Data.Distributive
import Data.Functor.Identity
import Data.Kind (Type)

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

instance (Alternative f, Applicative g) => Alternative (f :.: g) where
  empty = C empty
  {-# INLINE empty #-}

  C l <|> C r = C (l <|> r)
  {-# INLINE (<|>) #-}

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

assocLR :: (Functor f, Functor g) => ((f :.: g) :.: (h :.: i)) a -> ((f :.: (g :.: h)) :.: i) a
assocLR = mapC (mapC (fmap (C . fmap getC)))

assocRL :: (Functor f, Functor g) => ((f :.: (g :.: h)) :.: i) a -> ((f :.: g) :.: (h :.: i)) a
assocRL = mapC (mapC (fmap (fmap C . getC)))


weaken :: (Applicative m, Applicative i, Applicative j) => (m :.: i) a -> (m :.: i :.: j) a
weaken = weakens

strengthen :: Functor m => (m :.: Identity) a -> m a
strengthen = fmap runIdentity . getC


type Permutable f = (Applicative f, Distributive f)


class (Applicative m, Applicative n) => Extends m n where
  weakens :: m a -> n a

instance (Applicative f, Extends g1 g2) => Extends (f :.: g1) (f :.: g2) where
  weakens = mapC (fmap weakens)

instance (Applicative f, Applicative g) => Extends f (f :.: g) where
  weakens = liftC

instance Applicative f => Extends f f where
  weakens = id


newtype Tr (i :: Type -> Type) (j :: Type -> Type) k a = Tr { getTr :: k a }
  deriving (Applicative, Functor)

instance Distributive k => Distributive (Tr i j k) where
  distribute = Tr . distribute . fmap getTr
  {-# INLINE distribute #-}

  collect f = Tr . collect (getTr . f)
  {-# INLINE collect #-}

instance (Extends i j, Extends j k) => Extends i (Tr i j k) where
  weakens (m :: i a) = Tr (weakens (weakens m :: j a))
