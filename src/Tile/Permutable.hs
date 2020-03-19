{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Permutable
( (:.:)(..)
, Lam(..)
, ($$)
, var
, weaken
, Extends(..)
) where

import Control.Applicative (liftA2)

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


class Lam repr where
  lamPure :: (repr a -> repr b) -> repr (a -> b)
  appPure :: repr (a -> b) -> (repr a -> repr b)

($$) :: (Lam repr, Applicative m) => m (repr (a -> b)) -> (m (repr a) -> m (repr b))
($$) = liftA2 appPure

infixl 9 $$


var :: Applicative m => i (repr a) -> (m :.: i) (repr a)
var = C . pure

weaken :: (Applicative m, Applicative i, Applicative j) => (m :.: i) (repr a) -> (m :.: i :.: j) (repr a)
weaken = C . fmap (C . fmap pure) . getC


class (Applicative m, Applicative n) => Extends m n where
  weakens :: m a -> n a

instance (Applicative f, Extends g1 g2) => Extends (f :.: g1) (f :.: g2) where
  weakens = C . fmap weakens . getC

instance (Applicative f, Applicative g) => Extends f (f :.: g) where
  weakens = C . fmap pure
