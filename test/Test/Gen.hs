{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Gen
( var
, let'
, lam
, ($$)
, type'
, plicit
, Gen(..)
) where

import           Control.Applicative (liftA2)
import           Control.Monad.Reader
import           Hedgehog (MonadGen(..))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import           Tile.Plicit
import qualified Tile.Syntax as Syn
import           Tile.Type

var :: (Syn.Var v t, Functor m) => m v -> m t
var v = Syn.var <$> v

let' :: (Syn.Let v t, MonadReader Int m) => m t -> m (v -> t) -> m t
let' t b = Syn.let' <$> t <*> local succ b

lam :: (Syn.Lam v t, MonadGen m, MonadReader Int m) => m (v -> t) -> m t
lam b = Syn.lam <$> plicit <*> local succ b

($$) :: (Syn.Lam v t, Applicative m) => m t -> m t -> m t
f $$ a = (Syn.$$) <$> f <*> a

infixl 9 $$

type' :: (Syn.Type v t, Applicative m) => m t
type' = pure Syn.type'


plicit :: MonadGen m => m Plicit
plicit = Gen.enumBounded


newtype Gen a = Gen { runGen :: ReaderT Int Hedgehog.Gen a }
  deriving (Applicative, Functor, Monad, MonadGen, MonadReader Int)

instance Syn.Var Int t => Syn.Var Int (Gen t) where
  var = pure . Syn.var

instance Syn.Let Int t => Syn.Let Int (Gen t) where
  let' t b = Gen (Syn.let' <$> runGen t <*> (ask >>= fmap const . local succ . runGen . b))

instance Syn.Lam Int t => Syn.Lam Int (Gen t) where
  lam p b = Gen (Syn.lam p <$> (ask >>= fmap const . local succ . runGen . b))
  ($$) = liftA2 (Syn.$$)

instance Syn.Type Int t => Syn.Type Int (Gen t) where
  type' = pure Syn.type'
  (p, a) >-> b = Gen ((Syn.>->) . (,) p <$> runGen a <*> (ask >>= fmap const . local succ . runGen . b))

instance Syn.Prob Int t => Syn.Prob Int (Gen t) where
  ex t b = Gen (Syn.ex <$> runGen t <*> (ask >>= fmap const . local succ . runGen . b))
  (m1 ::: t1) === (m2 ::: t2) = Gen ((Syn.===) <$> ((:::) <$> runGen m1 <*> runGen t1) <*> ((:::) <$> runGen m2 <*> runGen t2))

instance Syn.Err t => Syn.Err (Gen t) where
  err = pure . Syn.err
