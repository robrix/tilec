{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Core
( Term(..)
, Spine(..)
, lam
, ($$)
, ($$*)
, pi'
) where

import Bound.Class
import Bound.Scope
import Control.Monad (ap)
import Data.Foldable (foldl')
import S.Syntax

data Term a
  = Abs (Scope () Term a)
  | a :$ Spine (Term a)
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Foldable, Functor, Traversable)

instance Applicative Term where
  pure = (:$ Nil)
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Abs b  -> Abs (b >>>= f)
    g :$ a -> f g $$* fmap (>>= f) a
    Type   -> Type
    Pi t b -> Pi (t >>= f) (b >>>= f)

infixl 9 :$


data Spine a
  = Nil
  | Spine a :> a
  deriving (Foldable, Functor, Traversable)

infixl 5 :>

instance Semigroup (Spine a) where
  a <> Nil      = a
  a <> (s :> b) = a <> s :> b

instance Monoid (Spine a) where
  mempty = Nil


lam :: Eq a => a -> Term a -> Term a
lam a b = Abs (abstract1 a b)

($$) :: Term a -> Term a -> Term a
t $$ a = case t of
  Abs b  -> instantiate1 a b
  f :$ s -> f :$ (s :> a)
  _      -> error "($$): illegal application"

infixl 9 $$

($$*) :: Foldable f => Term a -> f (Term a) -> Term a
($$*) = foldl' ($$)

infixl 9 $$*

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Pi t (abstract1 a b)
