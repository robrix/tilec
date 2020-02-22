{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Syntax
( Coalgebra(..)
, Project(..)
, Term(..)
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

class (HFunctor sig, Monad m) => Coalgebra sig m | m -> sig where
  coalg :: m a -> sig m a

class Project (sub :: (* -> *) -> (* -> *)) sup where
  prj :: sup m a -> Maybe (sub m a)

-- | Reflexivity: @t@ is a member of itself.
instance Project t t where
  prj = Just

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Project t (l1 :+: l2 :+: r)
      => Project t ((l1 :+: l2) :+: r) where
  prj = prj . reassoc where
    reassoc (L (L l)) = L l
    reassoc (L (R l)) = R (L l)
    reassoc (R r)     = R (R r)

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Project l (l :+: r) where
  prj (L l) = Just l
  prj _     = Nothing

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Project l r
      => Project l (l' :+: r) where
  prj (R r) = prj r
  prj _     = Nothing


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
