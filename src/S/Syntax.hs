{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Syntax
( Coalgebra(..)
, Project(..)
, Projects
, Is
, receive
, Term(..)
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
import Control.Monad.Trans.Class
import Data.Foldable (foldl')
import Data.Kind (Constraint)
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


type family Projects sub sup :: Constraint where
  Projects (l :+: r) u = (Projects l u, Projects r u)
  Projects t         u = Project t u

type Is eff sig m = (Projects eff sig, Coalgebra sig m)


receive :: (Project eff sig, Coalgebra sig m) => m a -> Maybe (eff m a)
receive = prj . coalg


newtype Term sig a = Term { unTerm :: sig (Term sig) a }
  deriving (Generic1)

deriving instance ( forall t . Foldable    t => Foldable    (sig t) ) => Foldable    (Term sig)
deriving instance ( forall t . Functor     t => Functor     (sig t) ) => Functor     (Term sig)
deriving instance ( forall t . Foldable    t => Foldable    (sig t)
                  , forall t . Functor     t => Functor     (sig t)
                  , forall t . Traversable t => Traversable (sig t) ) => Traversable (Term sig)

instance (RightModule sig, forall t . Functor t => Pointed (sig t)) => Applicative (Term sig) where
  pure = Term . point
  (<*>) = ap

instance (RightModule sig, forall t . Functor t => Pointed (sig t)) => Monad (Term sig) where
  Term a >>= f = Term (a >>=* f)

instance (RightModule sig, forall t . Functor t => Pointed (sig t)) => Algebra sig (Term sig) where
  alg = Term

instance (RightModule sig, forall t . Functor t => Pointed (sig t)) => Coalgebra sig (Term sig) where
  coalg = unTerm


data Expr t a
  = Abs (Scope t a)
  | a :$ Spine (t a)
  | Type
  | Pi (t a) (Scope t a)
  deriving (Foldable, Functor, Generic1, Traversable)

instance HFunctor Expr where
  hmap f = \case
    Abs b  -> Abs (hmap f b)
    a :$ s -> a :$ fmap f s
    Type   -> Type
    Pi t b -> Pi (f t) (hmap f b)

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

($$) :: (Is Expr prj t, Has Expr inj t) => t a -> t a -> t a
t $$ a = case receive t of
  Just (Abs b)  -> instantiate a b
  Just (f :$ s) -> send (f :$ (s :> a))
  _             -> error "($$): illegal application"

infixl 9 $$

($$*) :: (Foldable f, Is Expr prj t, Has Expr inj t) => t a -> f (t a) -> t a
($$*) = foldl' ($$)

infixl 9 $$*

type' :: Has Expr sig t => t a
type' = send Type

pi' :: (Eq a, Has Expr sig t) => a ::: t a -> t a -> t a
pi' (a ::: t) b = send (Pi t (abstract a b))


data a ::: b = a ::: b
  deriving (Foldable, Functor, Generic1, Traversable)

infix 0 :::


newtype Scope t a = Scope { unScope :: t (Maybe a) }
  deriving (Foldable, Functor, Generic1, Traversable)

instance HFunctor Scope where
  hmap f (Scope b) = Scope (f b)

instance Monad t => Applicative (Scope t) where
  pure = lift . pure
  (<*>) = ap

instance Monad t => Monad (Scope t) where
  Scope t >>= f = Scope (t >>= maybe (pure Nothing) (unScope . f))

instance MonadTrans Scope where
  lift = Scope . fmap Just

abstract :: (Functor t, Eq a) => a -> t a -> Scope t a
abstract a = Scope . fmap (\ a' -> if a == a' then Nothing else Just a')

instantiate :: Monad t => t a -> Scope t a -> t a
instantiate a t = unScope t >>= maybe a pure


class (HFunctor f, forall g . Functor g => Functor (f g)) => MonadAlgebra f where
  algM :: (Has f inj m, Is f prj m) => f m (m a) -> m a

class (HFunctor f, forall g . Functor g => Functor (f g)) => RightModule f where
  (>>=*) :: Monad m => f m a -> (a -> m b) -> f m b

  infixl 1 >>=*

instance (RightModule f, RightModule g) => RightModule (f :+: g) where
  s >>=* f = case s of
    L l -> L (l >>=* f)
    R r -> R (r >>=* f)


class Functor f => Pointed f where
  point :: a -> f a

instance (forall t . Functor t => Functor (r t), Functor t, Pointed (l t)) => Pointed ((l :+: r) t) where
  point = L . point
