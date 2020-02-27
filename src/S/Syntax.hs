{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( (:::)(..)
, pattern (:::)
, term_
, type_
, Stack(..)
, (!?)
) where

newtype a ::: b = Ascribe (a, b)
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance (Show a, Show b) => Show (a ::: b) where
  showsPrec p (a ::: b) = showParen (p > 0) (shows a . showString " ::: " . showsPrec 1 b)

pattern (:::) :: a -> b -> a ::: b
pattern a ::: b = Ascribe (a, b)

{-# COMPLETE (:::) #-}

infixl 0 :::

term_ :: a ::: b -> a
term_ (a ::: _) = a

type_ :: a ::: b -> b
type_ (_ ::: b) = b


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
