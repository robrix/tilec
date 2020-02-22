module S.Syntax
( Term(..)
) where

data Term a
  = Var a
  | Abs (Term (Maybe a))
  | Term a :$ Term a
