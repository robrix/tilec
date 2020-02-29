module Test.Gen
( var
, let'
, plicit
) where

import           Hedgehog (MonadGen(..))
import qualified Hedgehog.Gen as Gen
import           Tile.Plicit
import qualified Tile.Syntax as Syn

var :: (Syn.Var v t, MonadGen m) => m v -> m t
var v = Syn.var <$> v

let' :: (Syn.Let v t, MonadGen m) => m t -> m (v -> t) -> m t
let' t b = Syn.let' <$> t <*> b

plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded
