{-# LANGUAGE DeriveTraversable #-}
module S.Problem
( Term(..)
) where

import S.Scope

data Term a
  = Var a
  | Abs (Scope Term a)
  | Term a :$ Term a
  | Type
  | Pi (Term a) (Scope Term a)
  deriving (Foldable, Functor, Traversable)

infixl 9 :$
