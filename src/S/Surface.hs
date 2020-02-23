{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Surface
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
import Data.List (elemIndex)
import GHC.Generics (Generic1)
import S.Syntax

data Term a
  = Var a
  | Abs (Scope () Term a)
  | Term a :$ Term a
  | Let [Scope Int Term a] (Scope Int Term a)
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Foldable, Functor, Generic1, Traversable)

instance Eq1 Term where liftEq = liftEqDefault
instance Ord1 Term where liftCompare = liftCompareDefault
instance Show1 Term where liftShowsPrec = liftShowsPrecDefault

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

let' :: Eq a => [(a, Term a)] -> Term a -> Term a
let' [] b = b
let' vs b = Let (map (go . snd) vs) (go b) where
  go = abstract (`elemIndex` map fst vs)

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Pi t (abstract1 a b)
