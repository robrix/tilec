module S.Syntax.Free
( Term(..)
) where

data Term a
  = Var a
  | Let (Term a) (Term a) (a -> Term a)
  | Lam (a -> Term a)
  | Term a :$ Term a
  | Type
  | Pi (Term a) (a -> Term a)
