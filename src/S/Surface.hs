{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Surface
( Term(..)
, Var(..)
, Let(..)
, Lam(..)
, Lams(..)
, Type(..)
, (-->)
, Prob(..)
, Def(..)
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
  | Lam (Scope () Term a)
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
    Lam b     -> Lam (b >>>= f)
    g :$ a    -> (g >>= f) :$ (a >>= f)
    Let t v b -> Let (t >>= f) (v >>= f) (b >>>= f)
    Type      -> Type
    Pi t b    -> Pi (t >>= f) (b >>>= f)

infixl 9 :$


class Var expr a where
  var :: a -> expr a

class Var expr a => Let expr a where
  let' :: expr a ::: expr a -> (a -> expr a) -> expr a

class Var expr a => Lam expr a where
  lam :: (a -> expr a) -> expr a
  ($$) :: expr a -> expr a -> expr a

  infixl 9 $$

class Lam expr a => Lams expr a t | t -> expr a where
  lams :: t -> expr a

instance Lam expr a => Lams expr a (expr a) where
  lams = id

instance Lams expr a t => Lams expr a (a -> t) where
  lams f = lam (lams . f)

class Var expr a => Type expr a where
  type' :: expr a
  pi' :: expr a -> (a -> expr a) -> expr a

  infixr 0 `pi'`

(-->) :: Type expr a => expr a -> expr a -> expr a
a --> b = a `pi'` const b

infixr 0 -->

class Var expr a => Prob expr a where
  ex :: expr a -> (a -> expr a) -> expr a
  (===) :: expr a -> expr a -> expr a

  infixl 4 ===

class Def expr a def | def -> expr where
  def :: expr a ::: expr a -> def a
