{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Gen
( module Tile.Syntax
, genPlicit
, var_
, let_
, lam_
, app_
, type_
, pi_
, ex_
, eq_
, freeVariable_
, term_
, tag
) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Set
import           Hedgehog (LabelName, MonadGen(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Tile.Error
import           Tile.Plicit
import           Tile.Syntax

genPlicit :: MonadGen m => m Plicit
genPlicit = Gen.enumBounded

var_ :: (Var Int a t, MonadGen m, MonadReader Int m, MonadWriter (Set LabelName) m) => m (t a)
var_ = ask >>= \ i -> if i <= 0 then Gen.discard else var <$> Gen.int (Range.constant 0 i) <* tag "var"

let_ :: (MonadGen m, MonadReader Int m, Let v a t, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
let_ t = Gen.subtermM3 t t (local succ t) (\ v t b -> let' (v ::: t) (const b) <$ tag "let")

lam_ :: (MonadGen m, MonadReader Int m, Lam v a t, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
lam_ t = Gen.subtermM (local succ t) (\ b -> lam <$> genPlicit <*> pure (const b) <* tag "lam")

app_ :: (Lam v a t, MonadGen m, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
app_ t = Gen.subtermM2 t t (\ f a -> f $$ a <$ tag "$$")

type_ :: (Type v a t, MonadWriter (Set LabelName) m) => m (t a)
type_ = type' <$ tag "type"

pi_ :: (MonadGen m, MonadReader Int m, Type v a t, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
pi_ t = Gen.subtermM2 t (local succ t) (\ t b -> (>-> const b) . (, t) <$> genPlicit <* tag ">->")

ex_ :: (MonadGen m, MonadReader Int m, Prob v a t, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
ex_ t = Gen.subtermM2 t (local succ t) (\ t b -> (t `ex` const b) <$ tag "ex")

eq_ :: (MonadGen m, Prob v a t, MonadWriter (Set LabelName) m) => m (t a) -> m (t a)
eq_ t = Gen.subtermM2 t t (\ m1 t1 -> Gen.subtermM2 t t (\ m2 t2 -> ((m1 ::: t1) === (m2 ::: t2)) <$ tag "==="))

freeVariable_ :: (FreeVariable Int e, MonadGen m, MonadWriter (Set LabelName) m) => m e
freeVariable_ = freeVariable <$> Gen.int (Range.linear (-1) (-10)) <* tag "err"

term_ :: (Let Int a t, Lam Int a t, Type Int a t, Prob Int a t, FreeVariable Int e, Err e a t, MonadGen m, MonadReader Int m, MonadWriter (Set LabelName) m) => m (t a)
term_ = ask >>= \ i -> Gen.recursive (Gen.small . Gen.choice)
  ([ var_ | i > 0 ] <> [ type_, err <$> freeVariable_ ])
  ([ let_, lam_, app_, pi_, ex_, eq_ ] <*> [term_])


tag :: MonadWriter (Set LabelName) m => LabelName -> m ()
tag s = tell (singleton s)
