{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Problem
( Term(..)
, lam
, pi'
) where

import Bound.Class
import Bound.Scope
import Control.Monad (ap)
import S.Syntax

data Term a
  = Var a
  | Abs (Scope () Term a)
  | Term a :$ Term a
  | Let [Scope Int Term a] (Scope Int Term a)
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Foldable, Functor, Traversable)

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Var a   -> f a
    Abs b   -> Abs (b >>>= f)
    g :$ a  -> (g >>= f) :$ (a >>= f)
    Let v b -> Let (fmap (>>>= f) v) (b >>>= f)
    Type    -> Type
    Pi t b  -> Pi (t >>= f) (b >>>= f)

infixl 9 :$


lam :: Eq a => a -> Term a -> Term a
lam a b = Abs (abstract1 a b)

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Pi t (abstract1 a b)
