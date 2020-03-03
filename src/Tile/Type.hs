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
import Data.Functor.Classes

data a ::: b = a ::: b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable (:::) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (:::) where
  bimap = bimapDefault

instance Bitraversable (:::) where
  bitraverse f g (l ::: r) = (:::) <$> f l <*> g r

instance Show a => Show1 ((:::) a) where
  liftShowsPrec sp _ p (a ::: b) = showParen (p > 1) $ shows a . showString " ::: " . sp 2 b

infixl 1 :::

term_ :: a ::: b -> a
term_ (a ::: _) = a

type_ :: a ::: b -> b
type_ (_ ::: b) = b
