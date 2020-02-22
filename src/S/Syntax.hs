module S.Syntax
( Term(..)
, Spine(..)
) where

data Term a
  = Var a
  | Abs (Term (Maybe a))
  | Term a :$ Term a

infixl 9 :$

data Spine a
  = Nil
  | Spine a :> a

infixl 5 :>
