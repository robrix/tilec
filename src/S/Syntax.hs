{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax
( (:::)(..)
) where

data a ::: b = a ::: b
  deriving (Foldable, Functor, Traversable)

infix 0 :::
