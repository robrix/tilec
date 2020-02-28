{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Term
( Term(..)
, interpret
) where

import Control.Monad (ap, (>=>))
import Tile.Syntax

data Term v a
  = Var a
  | Let (Term v a) (v -> Term v a)
  | Lam Plicit (v -> Term v a)
  | Term v a :$ Term v a
  | Type
  | (Plicit, Term v a) :-> (v -> Term v a)
  | E (Term v a) (v -> Term v a)
  | (Term v a, Term v a) :===: (Term v a, Term v a)
  | Err String
  deriving (Functor)

instance Applicative (Term v) where
  pure = Var
  (<*>) = ap

instance Monad (Term v) where
  t >>= f = case t of
    Var a                   -> f a
    Let v b                 -> Let (v >>= f) (b >=> f)
    Lam p b                 -> Lam p (b >=> f)
    g :$ a                  -> (g >>= f) :$ (a >>= f)
    Type                    -> Type
    t :-> b                 -> fmap (>>= f) t :-> (b >=> f)
    E t b                   -> E (t >>= f) (b >=> f)
    (m1, t1) :===: (m2, t2) -> (m1 >>= f, t1 >>= f) :===: (m2 >>= f, t2 >>= f)
    Err s                   -> Err s

instance Var v (Term v v) where
  var = Var

instance Let v (Term v v) where
  let' = Let

instance Lam v (Term v v) where
  lam = Lam
  ($$) = (:$)

instance Type v (Term v v) where
  type' = Type
  (>->) = (:->)

instance Prob v (Term v v) where
  ex = E
  (m1 ::: t1) === (m2 ::: t2) = (m1, t1) :===: (m2, t2)

instance Err (Term v v) where
  err = Err

infixl 9 :$
infixr 0 :->
infixl 4 :===:

interpret :: (Let v t, Lam v t, Type v t, Prob v t, Err t) => Term v v -> t
interpret = \case
  Var v                   -> var v
  Let v b                 -> let' (interpret v) (interpret . b)
  Lam p b                 -> lam p (interpret . b)
  f :$ a                  -> interpret f $$ interpret a
  Type                    -> type'
  t :-> b                 -> fmap interpret t >-> interpret . b
  E t b                   -> interpret t `ex` interpret . b
  (m1, t1) :===: (m2, t2) -> (interpret m1 ::: interpret t1) === (interpret m2 ::: interpret t2)
  Err s                   -> err s
