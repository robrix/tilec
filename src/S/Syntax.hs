{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( Term(..)
, Prob(..)
, Expr(..)
, Spine(..)
, lam
, ($$)
, ($$*)
, type'
, pi'
) where

import Control.Algebra
import Control.Monad (ap)
import Data.Foldable (foldl')
import GHC.Generics (Generic1)

newtype Term a = Term { unTerm :: Expr Term a }
  deriving (Foldable, Functor, Generic1, Traversable)

instance Applicative Term where
  pure = Term . (:$ Nil)
  (<*>) = ap

instance Monad Term where
  Term a >>= f = case a of
    Abs b  -> Term (Abs (b >>= traverse f))
    g :$ s -> f g $$* fmap (>>= f) s
    Type   -> Term Type
    Pi t b -> Term (Pi (t >>= f) (b >>= traverse f))

instance Algebra Expr Term where
  alg = Term


data Prob a
  = Ex (Prob (Maybe a))
  | Prob (Expr Prob a)
  deriving (Foldable, Functor, Generic1, Traversable)

data Expr t a
  = Abs (t (Maybe a))
  | a :$ Spine (t a)
  | Type
  | Pi (t a) (t (Maybe a))
  deriving (Foldable, Functor, Generic1, Traversable)

instance HFunctor Expr where
  hmap f = \case
    Abs b  -> Abs (f b)
    a :$ s -> a :$ fmap f s
    Type   -> Type
    Pi t b -> Pi (f t) (f b)

infixl 9 :$

data Spine a
  = Nil
  | Spine a :> a
  deriving (Foldable, Functor, Generic1, Traversable)

infixl 5 :>

instance Semigroup (Spine a) where
  a <> Nil      = a
  a <> (s :> b) = a <> s :> b

instance Monoid (Spine a) where
  mempty = Nil


lam :: (Eq a, Has Expr sig t) => a -> t a -> t a
lam a b = send (Abs (abstract a b))

($$) :: Term a -> Term a -> Term a
t $$ a = case unTerm t of
  Abs b  -> instantiate a b
  f :$ s -> send (f :$ (s :> a))
  Type   -> error "($$): illegal application of Type"
  Pi _ _ -> error "($$): illegal application of Pi"

infixl 9 $$

($$*) :: Foldable t => Term a -> t (Term a) -> Term a
($$*) = foldl' ($$)

infixl 9 $$*

type' :: Has Expr sig t => t a
type' = send Type

pi' :: (Eq a, Has Expr sig t) => a ::: t a -> t a -> t a
pi' (a ::: t) b = send (Pi t (abstract a b))


data a ::: b = a ::: b
  deriving (Foldable, Functor, Generic1, Traversable)

infix 0 :::


abstract :: (Functor t, Eq a) => a -> t a -> t (Maybe a)
abstract a = fmap (\ a' -> if a == a' then Nothing else Just a')

instantiate :: Monad t => t a -> t (Maybe a) -> t a
instantiate a t = t >>= maybe a pure
