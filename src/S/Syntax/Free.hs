{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module S.Syntax.Free
( Term(..)
, interpret
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
  deriving (Functor)

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

interpret :: (Let t a, Lam t a, Type t a) => Term a a -> t a
interpret = \case
  Var v       -> var v
  Let tm ty b -> let' (interpret tm ::: interpret ty) (interpret . b)
  Lam b       -> lam (interpret . b)
  f :$ a      -> interpret f $$ interpret a
  Type        -> type'
  Pi t b      -> interpret t `pi'` interpret . b
