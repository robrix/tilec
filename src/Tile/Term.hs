{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tile.Term
( Term(..)
, interpret
) where

import Control.Monad (ap, (>=>))
import Tile.Syntax

data Term a b
  = Var b
  | Let (Term a b) (a -> Term a b)
  | Lam (a -> Term a b)
  | Term a b :$ Term a b
  | Type
  | Term a b :-> (a -> Term a b)
  | Term a b :. Term a b
  | Err String
  deriving (Functor)

instance Applicative (Term a) where
  pure = Var
  (<*>) = ap

instance Monad (Term a) where
  t >>= f = case t of
    Var a   -> f a
    Let v b -> Let (v >>= f) (b >=> f)
    Lam b   -> Lam (b >=> f)
    g :$ a  -> (g >>= f) :$ (a >>= f)
    Type    -> Type
    t :-> b -> (t >>= f) :-> (b >=> f)
    m :. t  -> (m >>= f) :. (t >>= f)
    Err s   -> Err s

instance Var a (Term a) where
  var = Var

instance Let a (Term a) where
  let' = Let

instance Lam a (Term a) where
  lam = Lam
  ($$) = (:$)

instance Type a (Term a) where
  type' = Type
  (>->) = (:->)
  (.:.) = (:.)

instance Err (Term a) where
  err = Err

infixl 9 :$
infixr 0 :->
infixl 0 :.

interpret :: (Let a t, Lam a t, Type a t, Err t) => Term a a -> t a
interpret = \case
  Var v   -> var v
  Let v b -> let' (interpret v) (interpret . b)
  Lam b   -> lam (interpret . b)
  f :$ a  -> interpret f $$ interpret a
  Type    -> type'
  t :-> b -> interpret t >-> interpret . b
  m :. t  -> interpret m .:. interpret t
  Err s   -> err s
