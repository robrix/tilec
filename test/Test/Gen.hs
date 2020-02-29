{-# LANGUAGE FlexibleContexts #-}
module Test.Gen
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, plicit
, localVar
) where

import           Control.Monad.Reader
import           Hedgehog (MonadGen(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Tile.Plicit
import           Tile.Syntax

plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded

localVar :: (Var Int t, MonadGen m, MonadReader Int m) => m t
localVar = ask >>= \ i -> if i <= 0 then Gen.discard else var <$> Gen.int (Range.constant 0 i)
