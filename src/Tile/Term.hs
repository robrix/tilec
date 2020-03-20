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
import Data.Functor.Classes
import Text.Show
import Tile.Syntax

data Term v a
  = Var a
  | Let (Term v a ::: Term v a) (v -> Term v a)
  | Lam Plicit (v -> Term v a)
  | Term v a :$ Term v a
  | Type
  | (Plicit, Term v a) :-> (v -> Term v a)
  | E (Term v a) (v -> Term v a)
  | (Term v a ::: Term v a) :===: (Term v a ::: Term v a)

instance (Eq v, Eq a, Num v) => Eq (Term v a) where
  (==) = eq 0 where
    eq n l r = case (l, r) of
      (Var v1, Var v2) -> v1 == v2
      (Let (v1 ::: t1) b1, Let (v2 ::: t2) b2) -> eq n v1 v2 && eq n t1 t2 && eq (n + 1) (b1 n) (b2 n)
      (Lam p1 b1, Lam p2 b2) -> p1 == p2 && eq (n + 1) (b1 n) (b2 n)
      (f1 :$ a1, f2 :$ a2) -> eq n f1 f2 && eq n a1 a2
      (Type, Type) -> True
      ((p1, a1) :-> b1, (p2, a2) :-> b2) -> p1 == p2 && eq n a1 a2 && eq (n + 1) (b1 n) (b2 n)
      (E t1 b1, E t2 b2) -> eq n t1 t2 && eq (n + 1) (b1 n) (b2 n)
      ((m11 ::: t11) :===: (m21 ::: t21), (m12 ::: t12) :===: (m22 ::: t22)) -> eq n m11 m12 && eq n t11 t12 && eq n m21 m22 && eq n t21 t22
      _ -> False

instance (Ord v, Ord a, Num v) => Ord (Term v a) where
  compare = cmp 0 where
    cmp n l r = case (l, r) of
      (Var v1, Var v2) -> v1 `compare` v2
      (Var{}, _) -> LT
      (Let (v1 ::: t1) b1, Let (v2 ::: t2) b2) -> cmp n v1 v2 <> cmp n t1 t2 <> cmp (n + 1) (b1 n) (b2 n)
      (Let{}, _) -> LT
      (Lam p1 b1, Lam p2 b2) -> p1 `compare` p2 <> cmp (n + 1) (b1 n) (b2 n)
      (Lam{}, _) -> LT
      (f1 :$ a1, f2 :$ a2) -> cmp n f1 f2 <> cmp n a1 a2
      ((:$){}, _) -> LT
      (Type, Type) -> EQ
      ((p1, a1) :-> b1, (p2, a2) :-> b2) -> p1 `compare` p2 <> cmp n a1 a2 <> cmp (n + 1) (b1 n) (b2 n)
      ((:->){}, _) -> LT
      (E t1 b1, E t2 b2) -> cmp n t1 t2 <> cmp (n + 1) (b1 n) (b2 n)
      (E{}, _) -> LT
      ((m11 ::: t11) :===: (m21 ::: t21), (m12 ::: t12) :===: (m22 ::: t22)) -> cmp n m11 m12 <> cmp n t11 t12 <> cmp n m21 m22 <> cmp n t21 t22
      ((:===:){}, _) -> LT
      _ -> GT

instance Functor (Term v) where
  fmap f = go where
    go = \case
      Var a       -> Var (f a)
      Let v b     -> Let (bimap go go v) (go . b)
      Lam p b     -> Lam p (go . b)
      f :$ a      -> go f :$ go a
      Type        -> Type
      a :-> b     -> fmap go a :-> go . b
      E t b       -> E (go t) (go . b)
      t1 :===: t2 -> bimap go go t1 :===: bimap go go t2

instance Num v => Foldable (Term v) where
  foldMap f = go 0 where
    go n = \case
      Var a       -> f a
      Let v b     -> bifoldMap (go n) (go n) v <> go (n + 1) (b n)
      Lam _ b     -> go (n + 1) (b n)
      f :$ a      -> go n f <> go n a
      Type        -> mempty
      a :-> b     -> foldMap (go n) a <> go (n + 1) (b n)
      E t b       -> go n t <> go (n + 1) (b n)
      t1 :===: t2 -> bifoldMap (go n) (go n) t1 <> bifoldMap (go n) (go n) t2

instance (Num v, Show v, Show a) => Show (Term v a) where
  showsPrec = go 0 where
    go n p = \case
      Var v -> showsUnaryWith showsPrec "Var" p v
      Let v b -> showsBinaryWith (showAnn n) (\ p b -> go (n + 1) p (b n)) "Let" p v b
      Lam x b -> showsBinaryWith showsPrec (\ p b -> go (n + 1) p (b n)) "Lam" p x b
      f :$ a -> showParen (p > 9) $ go n 9 f . showString " :$ " . go n 10 a
      Type -> showString "Type"
      a :-> b -> showParen (p > 0) $ liftShowsPrec (go n) (showListWith (go n 0)) 1 a . showString " :-> " . go (n + 1) 0 (b n)
      E t b -> showsBinaryWith (go n) (\ p b -> go (n + 1) p (b n)) "E" p t b
      t1 :===: t2 -> showParen (p > 4) $ showAnn n 4 t1 . showString " :===: " . showAnn n 5 t2
    showAnn n = liftShowsPrec (go n) (showListWith (go n 0))

instance Applicative (Term v) where
  pure = Var
  (<*>) = ap

instance Monad (Term v) where
  t >>= f = case t of
    Var a       -> f a
    Let v b     -> Let (bimap (>>= f) (>>= f) v) (b >=> f)
    Lam p b     -> Lam p (b >=> f)
    g :$ a      -> (g >>= f) :$ (a >>= f)
    Type        -> Type
    t :-> b     -> fmap (>>= f) t :-> (b >=> f)
    E t b       -> E (t >>= f) (b >=> f)
    t1 :===: t2 -> bimap (>>= f) (>>= f) t1 :===: bimap (>>= f) (>>= f) t2

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

infixl 9 :$
infixr 6 :->
infixl 4 :===:

interpret :: (Let v a, Lam v a, Type v a, Prob v a) => Term v v -> a
interpret = \case
  Var v       -> var v
  Let v b     -> let' (bimap interpret interpret v) (interpret . b)
  Lam p b     -> lam p (interpret . b)
  f :$ a      -> interpret f $$ interpret a
  Type        -> type'
  t :-> b     -> fmap interpret t >-> interpret . b
  E t b       -> interpret t `ex` interpret . b
  t1 :===: t2 -> bimap interpret interpret t1 === bimap interpret interpret t2
