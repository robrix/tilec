{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( (|-)
, elab
, Elab(..)
) where

import Data.Map
import Data.Maybe (fromMaybe)
import Tile.Context
import Tile.Syntax

(|-) :: Map v t -> Elab v t ::: t -> t
ctx |- (Elab m ::: t) = m t ctx

infixl 1 |-

elab :: Map v t :|-: Elab v t ::: t -> t
elab (ctx :|-: Elab m ::: t) = m t ctx

newtype Elab v t = Elab { runElabC :: t -> Map v t -> t }

instance (Ord v, Prob v t, Err v t) => Var v (Elab v t) where
  var n = check $ \ ctx -> pure (var n ::: typeOf ctx n)

instance (Ord v, Let v t, Prob v t, Type v t, Err v t) => Let v (Elab v t) where
  let' (v ::: t) b = check $ \ ctx -> do
    _B <- meta type'
    t' <- letbind ((ctx |- t ::: type')  ::: type')
    pure
      (   let' ((ctx |- v ::: var t') ::: var t') (\ x ->
            ctx |> x ::: var t' |- b x ::: var _B)
      ::: var _B)

instance (Ord v, Let v t, Lam v t, Prob v t, Type v t, Err v t) => Lam v (Elab v t) where
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

instance (Ord v, Let v t, Prob v t, Type v t, Err v t) => Type v (Elab v t) where
  type' = check (const (pure (type' ::: type')))

  (p, a) >-> b = check $ \ ctx -> do
    a' <- letbind ((ctx |- a ::: type') ::: type')
    pure
      (   (p, var a') >-> (\ x -> ctx |> x ::: var a' |- b x ::: type')
      ::: type')

deriving instance (Ord v, Prob v t, Err v t) => Prob v (Elab v t)

deriving instance Err v t => Err v (Elab v t)


typeOf :: (Ord v, Err v t) => Map v t -> v -> t
typeOf ctx n = fromMaybe (freeVariable n) (ctx !? n)

(|>) :: Ord v => Map v t -> v ::: t -> Map v t
ctx |> (v ::: t) = insert v t ctx

infixl 1 |>

check :: Prob v t => (Map v t -> Script t (t ::: t)) -> Elab v t
check f = Elab $ \ ty ctx -> runScript id $ do
  exp <- meta ty
  act <- f ctx
  pure $! var exp ::: ty === act
