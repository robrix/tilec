{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( (|-)
, ElabC(..)
) where

import Control.Carrier.Reader
import Data.Map
import Data.Maybe (fromMaybe)
import Tile.Error
import Tile.Syntax

(|-) :: Map v (m a) -> ElabC v a m b ::: m a -> m b
ctx |- (ElabC m ::: t) = runReader ctx (runReader t m)

infixl 1 |-

newtype ElabC v a m b = ElabC (ReaderC (m a) (ReaderC (Map v (m a)) m) b)
  deriving (Applicative, Functor, Monad)

instance (Ord v, Prob v a m, FreeVariable v e, Err e a m) => Var v a (ElabC v a m) where
  var n = check $ \ ctx -> pure (var n ::: typeOf ctx n)

instance (Ord v, Let v a m, Prob v a m, Type v a m, FreeVariable v e, Err e a m) => Let v a (ElabC v a m) where
  let' (v ::: t) b = check $ \ ctx -> do
    _B <- meta type'
    t' <- letbind ((ctx |- t ::: type')  ::: type')
    pure
      (   let' ((ctx |- v ::: var t') ::: var t') (\ x ->
            ctx |> x ::: var t' |- b x ::: var _B)
      ::: var _B)

instance (Ord v, Let v a m, Lam v a m, Prob v a m, Type v a m, FreeVariable v e, Err e a m) => Lam v a (ElabC v a m) where
  lam p b = check $ \ ctx -> do
    _A <- meta type'
    _B <- meta (var _A --> type')
    pure
      (   lam p (\ x -> ctx |> x ::: var _A |- b x ::: var _B $$ var x)
      ::: (p, var _A) >-> \ x -> var _B $$ var x)

  f $$ a = check $ \ ctx -> do
    _A <- meta type'
    _B <- meta type'
    pure
      (   (ctx |- f ::: var _A --> var _B) $$ (ctx |- a ::: var _A)
      ::: var _B)

instance (Ord v, Let v a m, Prob v a m, Type v a m, FreeVariable v e, Err e a m) => Type v a (ElabC v a m) where
  type' = check (const (pure (type' ::: type')))

  (p, a) >-> b = check $ \ ctx -> do
    a' <- letbind ((ctx |- a ::: type') ::: type')
    pure
      (   (p, var a') >-> (\ x -> ctx |> x ::: var a' |- b x ::: type')
      ::: type')

instance (Ord v, Let v a m, Prob v a m, Type v a m, FreeVariable v e, Err e a m) => Prob v a (ElabC v a m) where
  t `ex` b = check $ \ ctx -> do
    _B <- meta type'
    t' <- letbind ((ctx |- t ::: type') ::: type')
    pure
      (   var t' `ex` (\ x -> ctx |> x ::: var t' |- b x ::: var _B)
      ::: var _B)

  m1 ::: t1 === m2 ::: t2 = check $ \ ctx -> do
    t1' <- letbind ((ctx |- t1 ::: type') ::: type')
    t2' <- letbind ((ctx |- t2 ::: type') ::: type')
    pure
      (   (   (ctx |- m1 ::: var t1') ::: var t1'
          === (ctx |- m2 ::: var t2') ::: var t2')
      ::: (   var t1' ::: type'
          === var t2' ::: type'))

deriving instance Err e a m => Err e a (ElabC v a m)


typeOf :: (Ord v, FreeVariable v e, Err e a m) => Map v (m a) -> v -> m a
typeOf ctx n = fromMaybe (err (freeVariable n)) (ctx !? n)

(|>) :: Ord v => Map v t -> v ::: t -> Map v t
ctx |> (v ::: t) = insert v t ctx

infixl 1 |>

check :: Prob v a m => (Map v (m a) -> Script a m (m a ::: m a)) -> ElabC v a m a
check f = ElabC . ReaderC $ \ ty -> ReaderC $ \ ctx -> runScript id $ do
  exp <- meta ty
  act <- f ctx
  pure $! var exp ::: ty === act
