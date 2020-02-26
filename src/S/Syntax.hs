{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( (:::)(..)
, pattern (:::)
, term_
, type_
, (:=)(..)
, pattern (:=)
, Spine(..)
, (!?)
, N(..)
, Fin(..)
, Vec(..)
) where

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics (Generic1)

newtype a ::: b = Ascribe (a, b)
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance (Show a, Show b) => Show (a ::: b) where
  showsPrec p (a ::: b) = showParen (p > 0) (shows a . showString " ::: " . showsPrec 1 b)

pattern (:::) :: a -> b -> a ::: b
pattern a ::: b = Ascribe (a, b)

{-# COMPLETE (:::) #-}

infixl 0 :::

term_ :: a ::: b -> a
term_ (a ::: _) = a

type_ :: a ::: b -> b
type_ (_ ::: b) = b


newtype a := b = Define (a, b)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

pattern (:=) :: a -> b -> a := b
pattern a := b = Define (a, b)

{-# COMPLETE (:=) #-}

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

(!?) :: Spine a -> Int -> Maybe a
Nil        !? _ = Nothing
(_   :> a) !? 0 = Just a
(ctx :> _) !? n = ctx !? (n - 1)

infixl 9 !?


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
  (:.) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :.
