{-# LANGUAGE DeriveTraversable #-}
module S.Surface
( Term(..)
) where

import Bound.Class
import Bound.Scope
import Control.Monad (ap)

data Term a
  = Var a
  | Abs (Scope () Term a)
  | Term a :$ Term a
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Foldable, Functor, Traversable)

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Var a  -> f a
    Abs b  -> Abs (b >>>= f)
    g :$ a -> (g >>= f) :$ (a >>= f)
    Type   -> Type
    Pi t b -> Pi (t >>= f) (b >>>= f)

infixl 9 :$
