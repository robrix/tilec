{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Gen
( module Tile.Syntax
, plicit
, var_
, let_
, lam_
, app_
, type_
, pi_
, ex_
, eq_
, err_
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

var_ :: (Var Int t, MonadGen m, MonadReader Int m, MonadWriter (Set LabelName) m) => m t
var_ = ask >>= \ i -> if i <= 0 then Gen.discard else var <$> Gen.int (Range.constant 0 i) <* tag "var"

let_ :: (MonadGen m, MonadReader Int m, Let v t, MonadWriter (Set LabelName) m) => m t -> m t
let_ t = Gen.subtermM2 t (local succ t) (\ v b -> let' v (const b) <$ tag "let")

lam_ :: (MonadGen m, MonadReader Int m, Lam v t, MonadWriter (Set LabelName) m) => m t -> m t
lam_ t = Gen.subtermM (local succ t) (\ b -> lam <$> plicit <*> pure (const b) <* tag "lam")

app_ :: (Lam Int t, MonadGen m, MonadWriter (Set LabelName) m) => m t -> m t
app_ t = Gen.subtermM2 t t (\ f a -> f $$ a <$ tag "$$")

type_ :: (Type Int t, MonadWriter (Set LabelName) m) => m t
type_ = type' <$ tag "type"

pi_ :: (MonadGen m, MonadReader Int m, Type v t, MonadWriter (Set LabelName) m) => m t -> m t
pi_ t = Gen.subtermM2 t (local succ t) (\ t b -> (>-> const b) . (, t) <$> plicit <* tag ">->")

ex_ :: (MonadGen m, MonadReader Int m, Prob v t, MonadWriter (Set LabelName) m) => m t -> m t
ex_ t = Gen.subtermM2 t (local succ t) (\ t b -> (t `ex` const b) <$ tag "ex")

eq_ :: (MonadGen m, Prob v t, MonadWriter (Set LabelName) m) => m t -> m t
eq_ t = Gen.subtermM2 t t (\ m1 t1 -> Gen.subtermM2 t t (\ m2 t2 -> ((m1 ::: t1) === (m2 ::: t2)) <$ tag "==="))

err_ :: (Err t, MonadGen m, MonadWriter (Set LabelName) m) => m t
err_ = err <$> Gen.string (Range.linear 0 10) Gen.alphaNum <* tag "err"


tag :: MonadWriter (Set LabelName) m => LabelName -> m ()
tag s = tell (singleton s)
