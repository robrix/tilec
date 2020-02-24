{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( (:::)(..)
, (:=)(..)
, Spine(..)
, N(..)
, Fin(..)
, Vec(..)
) where

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics (Generic1)

data a ::: b = a ::: b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 0 :::


data a := b = a := b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 0 :=


data Spine a
  = Nil
  | Spine a :> a
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

infixl 5 :>

instance Semigroup (Spine a) where
  a <> Nil      = a
  a <> (s :> b) = a <> s :> b

instance Monoid (Spine a) where
  mempty = Nil

instance Eq1 Spine where liftEq = liftEqDefault
instance Ord1 Spine where liftCompare = liftCompareDefault
instance Show1 Spine where liftShowsPrec = liftShowsPrecDefault


data N = Z | S N
  deriving (Eq, Ord, Show)


data Fin (n :: N) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)


data Vec (n :: N) a where
  VZ :: Vec 'Z a
  VS :: a -> Vec n a -> Vec ('S n) a
