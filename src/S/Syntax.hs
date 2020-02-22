{-# LANGUAGE DeriveTraversable #-}
module S.Syntax
( Term(..)
, Spine(..)
) where

data Term a
  = Abs (Term (Maybe a))
  | a :$ Spine (Term a)
  deriving (Foldable, Functor, Traversable)

infixl 9 :$

data Spine a
  = Nil
  | Spine a :> a
  deriving (Foldable, Functor, Traversable)

infixl 5 :>
