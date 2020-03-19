{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Permutable
( (:.:)(..)
, Lam(..)
, ($$)
) where

import Control.Applicative (liftA2)

newtype (f :.: g) a = C { getC :: f (g a) }
  deriving (Functor)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  {-# INLINE pure #-}

  C f <*> C a = C (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}


class Lam repr where
  lamPure :: (repr a -> repr b) -> repr (a -> b)
  appPure :: repr (a -> b) -> (repr a -> repr b)

($$) :: (Lam repr, Applicative m) => m (repr (a -> b)) -> (m (repr a) -> m (repr b))
($$) = liftA2 appPure

infixl 9 $$
