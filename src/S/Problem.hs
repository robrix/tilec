{-# LANGUAGE DeriveTraversable #-}
module S.Problem
( Term(..)
) where

import Control.Monad (ap)
import Control.Monad.Trans.Class
import S.Scope

data Term a
  = Var a
  | Abs (Scope Term a)
  | Term a :$ Term a
  | Type
  | Pi (Term a) (Scope Term a)
  deriving (Foldable, Functor, Traversable)

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  t >>= f = case t of
    Var a  -> f a
    Abs b  -> Abs (b >>= lift . f)
    g :$ a -> (g >>= f) :$ (a >>= f)
    Type   -> Type
    Pi t b -> Pi (t >>= f) (b >>= lift . f)

infixl 9 :$
