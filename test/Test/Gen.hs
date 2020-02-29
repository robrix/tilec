{-# LANGUAGE FlexibleContexts #-}
module Test.Gen
( var
, let'
, lam
, plicit
) where

import           Control.Monad.Reader
import           Hedgehog (MonadGen(..))
import qualified Hedgehog.Gen as Gen
import           Tile.Plicit
import qualified Tile.Syntax as Syn

var :: (Syn.Var v t, MonadGen m) => m v -> m t
var v = Syn.var <$> v

let' :: (Syn.Let v t, MonadGen m, MonadReader Int m) => m t -> m (v -> t) -> m t
let' t b = Syn.let' <$> t <*> local succ b

lam :: (Syn.Lam v t, MonadGen m, MonadReader Int m) => m (v -> t) -> m t
lam b = Syn.lam <$> plicit <*> local succ b


plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded
