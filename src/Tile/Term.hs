{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Term
( Term(..)
, interpret
) where

import Control.Monad (ap, (>=>))
import Data.Bifoldable
import Data.Bifunctor
import Tile.Syntax

data Term v a
  = Var a
  | Let (Term v a) (v -> Term v a)
  | Lam Plicit (v -> Term v a)
  | Term v a :$ Term v a
  | Type
  | (Plicit, Term v a) :-> (v -> Term v a)
  | E (Term v a) (v -> Term v a)
  | (Term v a ::: Term v a) :===: (Term v a ::: Term v a)
  | Err String

instance Functor (Term v) where
  fmap f = go where
    go = \case
      Var a       -> Var (f a)
      Let v b     -> Let (go v) (go . b)
      Lam p b     -> Lam p (go . b)
      f :$ a      -> go f :$ go a
      Type        -> Type
      a :-> b     -> fmap go a :-> go . b
      E t b       -> E (go t) (go . b)
      t1 :===: t2 -> bimap go go t1 :===: bimap go go t2
      Err s       -> Err s

instance Num v => Foldable (Term v) where
  foldMap f = go 0 where
    go n = \case
      Var a       -> f a
      Let v b     -> go n v <> go (n + 1) (b n)
      Lam _ b     -> go (n + 1) (b n)
      f :$ a      -> go n f <> go n a
      Type        -> mempty
      a :-> b     -> foldMap (go n) a <> go (n + 1) (b n)
      E t b       -> go n t <> go (n + 1) (b n)
      t1 :===: t2 -> bifoldMap (go n) (go n) t1 <> bifoldMap (go n) (go n) t2
      Err _       -> mempty

instance Applicative (Term v) where
  pure = Var
  (<*>) = ap

instance Monad (Term v) where
  t >>= f = case t of
    Var a       -> f a
    Let v b     -> Let (v >>= f) (b >=> f)
    Lam p b     -> Lam p (b >=> f)
    g :$ a      -> (g >>= f) :$ (a >>= f)
    Type        -> Type
    t :-> b     -> fmap (>>= f) t :-> (b >=> f)
    E t b       -> E (t >>= f) (b >=> f)
    t1 :===: t2 -> bimap (>>= f) (>>= f) t1 :===: bimap (>>= f) (>>= f) t2
    Err s       -> Err s

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
  (===) = (:===:)

instance Err (Term v v) where
  err = Err

infixl 9 :$
infixr 0 :->
infixl 4 :===:

interpret :: (Let v t, Lam v t, Type v t, Prob v t, Err t) => Term v v -> t
interpret = \case
  Var v       -> var v
  Let v b     -> let' (interpret v) (interpret . b)
  Lam p b     -> lam p (interpret . b)
  f :$ a      -> interpret f $$ interpret a
  Type        -> type'
  t :-> b     -> fmap interpret t >-> interpret . b
  E t b       -> interpret t `ex` interpret . b
  t1 :===: t2 -> bimap interpret interpret t1 === bimap interpret interpret t2
  Err s       -> err s
