module S.Surface
( Term(..)
) where

import S.Scope

data Term a
  = Var a
  | Abs (Scope Term a)
  | Term a :$ Term a

infixl 9 :$
