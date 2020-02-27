{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Core
( Term(..)
, lam
, ($$)
, ($$*)
, let'
, pi'
) where

import Bound.Class
import Bound.Scope
import Control.Monad (ap)
import Data.Foldable (foldl')
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics (Generic1)
import S.Syntax

data Term a
  = Lam (Scope () Term a)
  | a :$ Stack (Term a)
  | Let (Term a) (Term a) (Scope () Term a)
  | Type
  | Pi (Term a) (Scope () Term a)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance Eq1 Term where liftEq = liftEqDefault
instance Ord1 Term where liftCompare = liftCompareDefault
instance Show1 Term where liftShowsPrec = liftShowsPrecDefault

instance Applicative Term where
  pure = (:$ Nil)
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Lam b     -> Lam (b >>>= f)
    g :$ a    -> f g $$* fmap (>>= f) a
    Let t v b -> Let (t >>= f) (v >>= f) (b >>>= f)
    Type      -> Type
    Pi t b    -> Pi (t >>= f) (b >>>= f)

infixl 9 :$


lam :: Eq a => a -> Term a -> Term a
lam a b = Lam (abstract1 a b)

($$) :: Term a -> Term a -> Term a
t $$ a = case t of
  Lam b  -> instantiate1 a b
  f :$ s -> f :$ (s :> a)
  _      -> error "($$): illegal application"

infixl 9 $$

($$*) :: Foldable f => Term a -> f (Term a) -> Term a
($$*) = foldl' ($$)

infixl 9 $$*

let' :: Eq a => a ::: Term a := Term a -> Term a -> Term a
let' (a ::: t := v) b = Let t v (abstract1 a b)

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Pi t (abstract1 a b)
