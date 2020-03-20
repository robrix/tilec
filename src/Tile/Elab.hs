{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( (|-)
, ElabC(ElabC)
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Throw
import Data.Map
import Data.Maybe (fromMaybe)
import Tile.Functor.Compose
import Tile.Syntax
import qualified Tile.Syntax.Lifted as L

(|-) :: Map v (m a) -> ElabC v a m a ::: m a -> m a
ctx |- (m ::: t) = runElab t ctx m

infixl 1 |-

runElab :: m a -> Map v (m a) -> ElabC v a m b -> m b
runElab ty ctx (ElabC run) = run ty ctx

newtype ElabC v t m a = ElabC (m t -> Map v (m t) -> m a)
  deriving (Applicative, Functor, Monad) via ReaderC (m t) (ReaderC (Map v (m t)) m)

newtype ElabC' v t m a = ElabC' (t -> Map v t -> m a)
  deriving (Applicative, Functor, Monad) via ReaderC t (ReaderC (Map v t) m)

instance Algebra sig m => Algebra sig (ElabC v a m) where
  alg hdl sig ctx = ElabC $ \ ty env -> alg (runElab ty env . hdl) sig ctx

instance (Ord v, Show v, Prob v (m a), MonadFail m) => Var v (ElabC v a m a) where
  var n = check $ \ ctx -> pure (var n ::: typeOf ctx n)

instance (Ord (i v), Prob v t, Has (Throw (Err (i v))) sig m, L.Permutable i) => Var (i v) (ElabC' (i v) t (m :.: i) t) where
  var n = ElabC' $ \ ty ctx ->
    pure ty `L.ex` \ exp ->
    L.var exp ::: pure ty
    L.===
    weaken (L.var n) ::: weaken (typeOf' ctx n)

instance (Ord v, Show v, Let v (m a), Prob v (m a), Type v (m a), MonadFail m) => Let v (ElabC v a m a) where
  let' (v ::: t) b = check $ \ ctx -> do
    _B <- meta type'
    t' <- letbind ((ctx |- t ::: type') ::: type')
    pure
      (   let' ((ctx |- v ::: var t') ::: var t') (\ x ->
            ctx |> x ::: var t' |- b x ::: var _B)
      ::: var _B)

instance (Ord v, Show v, Let v (m a), Lam v (m a), Prob v (m a), Type v (m a), MonadFail m) => Lam v (ElabC v a m a) where
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

instance (Ord v, Show v, Let v (m a), Prob v (m a), Type v (m a), MonadFail m) => Type v (ElabC v a m a) where
  type' = check (const (pure (type' ::: type')))

  (p, a) >-> b = check $ \ ctx -> do
    a' <- letbind ((ctx |- a ::: type') ::: type')
    pure
      (   (p, var a') >-> (\ x -> ctx |> x ::: var a' |- b x ::: type')
      ::: type')

instance (Ord v, Show v, Let v (m a), Prob v (m a), Type v (m a), MonadFail m) => Prob v (ElabC v a m a) where
  t `ex` b = check $ \ ctx -> do
    _B <- meta type'
    t' <- letbind ((ctx |- t ::: type') ::: type')
    pure
      (   var t' `ex` (\ x -> ctx |> x ::: var t' |- b x ::: var _B)
      ::: var _B)

  (m1 ::: t1) === (m2 ::: t2) = check $ \ ctx -> do
    t1' <- letbind ((ctx |- t1 ::: type') ::: type')
    t2' <- letbind ((ctx |- t2 ::: type') ::: type')
    pure
      (   (   (ctx |- m1 ::: var t1') ::: var t1'
          === (ctx |- m2 ::: var t2') ::: var t2')
      ::: (   var t1' ::: type'
          === var t2' ::: type'))


typeOf' :: (Ord (i v), Has (Throw (Err (i v))) sig m, Applicative i) => Map (i v) t -> i v -> (m :.: i) t
typeOf' ctx n = maybe (C (throwError (FreeVariable n))) pure (ctx !? n)

typeOf :: (Ord v, Show v, MonadFail m) => Map v (m a) -> v -> m a
typeOf ctx n = fromMaybe (fail ("free variable:" <> show n)) (ctx !? n)

(|>) :: Ord v => Map v t -> v ::: t -> Map v t
ctx |> (v ::: t) = insert v t ctx

infixl 1 |>

check :: Prob v (m a) => (Map v (m a) -> Script (m a) (m a ::: m a)) -> ElabC v a m a
check f = ElabC $ \ ty ctx -> runScript id $ do
  exp <- meta ty
  act <- f ctx
  pure $! var exp ::: ty === act


newtype Err v
  = FreeVariable v
  deriving (Eq, Ord, Show)
