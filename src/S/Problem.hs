{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Problem
( Term(..)
, lam
, let'
, pi'
) where

import Bound.Class
import Bound.Scope
import Control.Monad (ap)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics (Generic1)
import S.Syntax

data Term a
  = Var a
  | Abs (Scope () Term a)
  | Term a :$ Term a
  | Let (Term a) (Term a) (Scope () Term a)
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq1 Term where liftEq = liftEqDefault
instance Ord1 Term where liftCompare = liftCompareDefault
instance Show1 Term where liftShowsPrec = liftShowsPrecDefault

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Var a     -> f a
    Abs b     -> Abs (b >>>= f)
    g :$ a    -> (g >>= f) :$ (a >>= f)
    Let t v b -> Let (t >>= f) (v >>= f) (b >>>= f)
    Type      -> Type
    Pi t b    -> Pi (t >>= f) (b >>>= f)

infixl 9 :$


lam :: Eq a => a -> Term a -> Term a
lam a b = Abs (abstract1 a b)

let' :: Eq a => a ::: Term a := Term a -> Term a -> Term a
let' (a ::: t := v) b = Let t v (abstract1 a b)

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Pi t (abstract1 a b)
