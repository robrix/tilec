{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module S.Type
( (:::)(..)
, pattern (:::)
, term_
, type_
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
