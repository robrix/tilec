{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Gen
( var
, let'
, lam
, ($$)
, type'
, plicit
, Gen(..)
) where

import           Control.Monad.Reader
import           Hedgehog (MonadGen(..))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import           Tile.Plicit
import qualified Tile.Syntax as Syn

var :: (Syn.Var v t, Functor m) => m v -> m t
var v = Syn.var <$> v

let' :: (Syn.Let v t, MonadReader Int m) => m t -> m (v -> t) -> m t
let' t b = Syn.let' <$> t <*> local succ b

lam :: (Syn.Lam v t, MonadGen m, MonadReader Int m) => m (v -> t) -> m t
lam b = Syn.lam <$> plicit <*> local succ b

($$) :: (Syn.Lam v t, Applicative m) => m t -> m t -> m t
f $$ a = (Syn.$$) <$> f <*> a

infixl 9 $$

type' :: (Syn.Type v t, Applicative m) => m t
type' = pure Syn.type'


plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded


newtype Gen a = Gen { runGen :: ReaderT Int Hedgehog.Gen a }
  deriving (Applicative, Functor, Monad, MonadGen)
