module S.Syntax
( Term(..)
, Spine(..)
) where

data Term a
  = Abs (Term (Maybe a))
  | a :$ Spine (Term a)

infixl 9 :$

data Spine a
  = Nil
  | Spine a :> a

infixl 5 :>
