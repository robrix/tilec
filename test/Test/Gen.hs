module Test.Gen
( var
, let'
) where

import           Control.Monad.Trans.Reader
import           Hedgehog (Gen)
import qualified Tile.Syntax as Syn

var :: Syn.Var v t => ReaderT Int Gen v -> ReaderT Int Gen t
var v = Syn.var <$> v

let' :: Syn.Let v t => ReaderT Int Gen t -> ReaderT Int Gen (v -> t) -> ReaderT Int Gen t
let' t b = Syn.let' <$> t <*> b
