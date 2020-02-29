module Test.Gen
( var
) where

import           Control.Monad.Trans.Reader
import           Hedgehog (Gen)
import qualified Tile.Syntax as Syn

var :: Syn.Var v t => ReaderT Int Gen v -> ReaderT Int Gen t
var v = Syn.var <$> v
