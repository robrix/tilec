{-# LANGUAGE DeriveTraversable #-}
module Tile.Stack
( Stack(..)
, (!?)
) where

data Stack a
  = Nil
  | Stack a :> a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 5 :>

instance Semigroup (Stack a) where
  a <> Nil      = a
  a <> (s :> b) = a <> s :> b

instance Monoid (Stack a) where
  mempty = Nil

(!?) :: Stack a -> Int -> Maybe a
Nil        !? _ = Nothing
(_   :> a) !? 0 = Just a
(ctx :> _) !? n = ctx !? (n - 1)

infixl 9 !?
