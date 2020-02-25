{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module S.Syntax.Free
( Term(..)
) where

import S.Syntax
import S.Syntax.Classes

data Term a b
  = Var b
  | Let (Term a b) (Term a b) (a -> Term a b)
  | Lam (a -> Term a b)
  | Term a b :$ Term a b
  | Type
  | Pi (Term a b) (a -> Term a b)

instance Var (Term a) a where
  var = Var

instance Let (Term a) a where
  let' (tm ::: ty) = Let tm ty

instance Lam (Term a) a where
  lam = Lam
  ($$) = (:$)

instance Type (Term a) a where
  type' = Type
  pi' = Pi

infixl 9 :$
