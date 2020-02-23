{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( (:::)(..)
, Spine(..)
) where

import GHC.Generics (Generic1)

data a ::: b = a ::: b
  deriving (Foldable, Functor, Traversable)

infix 0 :::


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
