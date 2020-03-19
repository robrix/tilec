{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Functor.Compose
( (:.:)(..)
, liftC
, mapC
, assocL
, assocR
, assocLR
, assocRL
) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Coerce (coerce)
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

instance (Alternative f, Applicative g) => Alternative (f :.: g) where
  empty = C empty

  C l <|> C r = C (l <|> r)

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
