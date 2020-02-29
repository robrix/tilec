{-# LANGUAGE FlexibleContexts #-}
module Test.Gen
( module Tile.Syntax
, plicit
, localVar
, tag
) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Set
import           Hedgehog (LabelName, MonadGen(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Tile.Plicit
import           Tile.Syntax

plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded

localVar :: (Var Int t, MonadGen m, MonadReader Int m) => m t
localVar = ask >>= \ i -> if i <= 0 then Gen.discard else var <$> Gen.int (Range.constant 0 i)

tag :: MonadWriter (Set LabelName) m => LabelName -> m ()
tag s = tell (singleton s)
