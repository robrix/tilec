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
  | Lam Plicit (a -> Term a b)
  | Term a b :$ Term a b
  | Type
  | (Plicit, Term a b) :-> (a -> Term a b)
  | E (Term a b) (a -> Term a b)
  | Err String
  deriving (Functor)

instance Applicative (Term a) where
  pure = Var
  (<*>) = ap

instance Monad (Term a) where
  t >>= f = case t of
    Var a   -> f a
    Let v b -> Let (v >>= f) (b >=> f)
    Lam p b -> Lam p (b >=> f)
    g :$ a  -> (g >>= f) :$ (a >>= f)
    Type    -> Type
    t :-> b -> fmap (>>= f) t :-> (b >=> f)
    E t b   -> E (t >>= f) (b >=> f)
    Err s   -> Err s

instance Var a (Term a a) where
  var = Var

instance Let a (Term a a) where
  let' = Let

instance Lam a (Term a a) where
  lam = Lam
  ($$) = (:$)

instance Type a (Term a a) where
  type' = Type
  (>->) = (:->)

instance Err (Term a a) where
  err = Err

infixl 9 :$
infixr 0 :->

interpret :: (Let a t, Lam a t, Type a t, Prob a t, Err t) => Term a a -> t
interpret = \case
  Var v   -> var v
  Let v b -> let' (interpret v) (interpret . b)
  Lam p b -> lam p (interpret . b)
  f :$ a  -> interpret f $$ interpret a
  Type    -> type'
  t :-> b -> fmap interpret t >-> interpret . b
  E t b   -> interpret t `ex` interpret . b
  Err s   -> err s
