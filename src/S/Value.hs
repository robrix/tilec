module S.Value
( Value(..)
) where

import S.Syntax

data Value a
  = Lam (Value a -> Value a)
  | a :$ Spine (Value a)
  | Type
  | Pi (Value a) (Value a -> Value a)
