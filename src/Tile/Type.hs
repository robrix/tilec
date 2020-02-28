{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Type
( (:::)(..)
, term_
, type_
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

data a ::: b = a ::: b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable (:::) where
  bifoldMap f g (l ::: r) = f l <> g r

instance Bifunctor (:::) where
  bimap f g (l ::: r) = f l ::: g r

instance Bitraversable (:::) where
  bitraverse f g (l ::: r) = (:::) <$> f l <*> g r

infixl 0 :::

term_ :: a ::: b -> a
term_ (a ::: _) = a

type_ :: a ::: b -> b
type_ (_ ::: b) = b
