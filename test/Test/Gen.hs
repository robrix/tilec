module Test.Gen
( var
, let'
, plicit
) where

import           Control.Monad.Trans.Reader
import           Hedgehog (Gen, MonadGen(..))
import qualified Hedgehog.Gen as Gen
import           Tile.Plicit
import qualified Tile.Syntax as Syn

var :: Syn.Var v t => ReaderT Int Gen v -> ReaderT Int Gen t
var v = Syn.var <$> v

let' :: Syn.Let v t => ReaderT Int Gen t -> ReaderT Int Gen (v -> t) -> ReaderT Int Gen t
let' t b = Syn.let' <$> t <*> b

plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded
