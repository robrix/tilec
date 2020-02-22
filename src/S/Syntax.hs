{-# LANGUAGE DeriveTraversable #-}
module S.Syntax
( Term(..)
, Prob(..)
, Expr(..)
, Spine(..)
) where

newtype Term a = Term (Expr Term a)
  deriving (Foldable, Functor, Traversable)

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
