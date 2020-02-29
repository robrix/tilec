{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Gen
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, plicit
, localVar
, Gen(..)
) where

import           Control.Applicative (liftA2)
import           Control.Monad.Reader
import           Hedgehog (MonadGen(..))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Tile.Plicit
import           Tile.Syntax

plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded

localVar :: (Var Int (m t), MonadGen m, MonadReader Int m) => m t
localVar = ask >>= \ i -> if i <= 0 then Gen.discard else Gen.int (Range.constant 0 i) >>= var


newtype Gen a = Gen { runGen :: ReaderT Int Hedgehog.Gen a }
  deriving (Applicative, Functor, Monad, MonadGen, MonadReader Int)

instance Var Int t => Var Int (Gen t) where
  var = pure . var

instance Let Int t => Let Int (Gen t) where
  let' t b = Gen (let' <$> runGen t <*> (ask >>= fmap const . local succ . runGen . b))

instance Lam Int t => Lam Int (Gen t) where
  lam p b = Gen (lam p <$> (ask >>= fmap const . local succ . runGen . b))
  ($$) = liftA2 ($$)

instance Type Int t => Type Int (Gen t) where
  type' = pure type'
  (p, a) >-> b = Gen ((>->) . (,) p <$> runGen a <*> (ask >>= fmap const . local succ . runGen . b))

instance Prob Int t => Prob Int (Gen t) where
  ex t b = Gen (ex <$> runGen t <*> (ask >>= fmap const . local succ . runGen . b))
  (m1 ::: t1) === (m2 ::: t2) = Gen ((===) <$> ((:::) <$> runGen m1 <*> runGen t1) <*> ((:::) <$> runGen m2 <*> runGen t2))

instance Err t => Err (Gen t) where
  err = pure . err
