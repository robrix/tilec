{-# LANGUAGE DeriveTraversable #-}
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

import Control.Monad (ap)
import Data.Foldable (foldl')

newtype Term a = Term (Expr Term a)
  deriving (Foldable, Functor, Traversable)

instance Applicative Term where
  pure = Term . (:$ Nil)
  (<*>) = ap

instance Monad Term where
  Term a >>= f = case a of
    Abs b  -> Term (Abs (b >>= traverse f))
    g :$ s -> f g $$* fmap (>>= f) s
    Type   -> Term Type
    Pi t b -> Term (Pi (t >>= f) (b >>= traverse f))


data Prob a
  = Ex (Prob (Maybe a))
  | Prob (Expr Prob a)
  deriving (Foldable, Functor, Traversable)

data Expr t a
  = Abs (t (Maybe a))
  | a :$ Spine (t a)
  | Type
  | Pi (t a) (t (Maybe a))
  deriving (Foldable, Functor, Traversable)

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
lam a b = Term (Abs (abstract a b))

($$) :: Term a -> Term a -> Term a
Term (Abs b)  $$ a = instantiate a b
Term (f :$ s) $$ a = Term (f :$ (s :> a))
Term Type     $$ _ = error "($$): illegal application of Type"
Term (Pi _ _) $$ _ = error "($$): illegal application of Pi"

infixl 9 $$

($$*) :: Foldable t => Term a -> t (Term a) -> Term a
($$*) = foldl' ($$)

infixl 9 $$*

type' :: Term a
type' = Term Type

pi' :: Eq a => a ::: Term a -> Term a -> Term a
pi' (a ::: t) b = Term (Pi t (abstract a b))


data a ::: b = a ::: b
  deriving (Foldable, Functor, Traversable)

infix 0 :::


abstract :: (Functor t, Eq a) => a -> t a -> t (Maybe a)
abstract a = fmap (\ a' -> if a == a' then Nothing else Just a')

instantiate :: Monad t => t a -> t (Maybe a) -> t a
instantiate a t = t >>= maybe a pure
