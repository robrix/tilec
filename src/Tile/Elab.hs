{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
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
( runElab
, Elab(..)
, elab
, runScript
, Script(..)
, meta
, intro
, letbind
) where

import Control.Carrier.Reader
import Control.Monad (ap)
import Data.Map
import Data.Maybe (fromMaybe)
import Tile.Context
import Tile.Syntax

runElab :: Map v t :|-: Elab v t t ::: t -> t
runElab (ctx :|-: Elab m ::: t) = runReader t m ctx

newtype Elab v t a = Elab { runElabC :: ReaderC t ((->) (Map v t)) a }
  deriving (Applicative, Functor)

instance (Ord v, Show v, Prob v t, Err t) => Var v (Elab v t t) where
  var n = Elab . ReaderC $ \ ty ctx ->
    ty `ex` \ res ->
    (var res ::: ty)
    ===
    (var n ::: fromMaybe (err ("free variable: " <> show n)) (ctx !? n))

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Let v (Elab v t t) where
  let' (v ::: t) b = Elab . ReaderC $ \ ty ctx ->
    ty `ex` \ res ->
    type' `ex` \ _B ->
    (var res ::: ty)
    ===
    ( let' ((ctx |- t ::: type')  ::: type')  (\ t' ->
      let' ((ctx |- v ::: var t') ::: var t') (\ x ->
        ctx |> x ::: var t' |- b x ::: var _B))
    ::: var _B)

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam p b = Elab . ReaderC $ \ ty ctx ->
    ty `ex` \ res ->
    type' `ex` \ _A ->
    (var _A --> type') `ex` \ _B ->
    (var res ::: ty)
    ===
    (lam p (\ x -> ctx |> x ::: var _A |- b x ::: var _B $$ var x) ::: (p, var _A) >-> \ x -> var _B $$ var x)

  f $$ a = Elab . ReaderC $ \ ty ctx ->
    ty `ex` \ res ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    (var res ::: ty)
    ===
    ((ctx |- f ::: var _A --> var _B) $$ (ctx |- a ::: var _A) ::: var _B)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = Elab . ReaderC $ \ _ _ -> type'

  (p, a) >-> b = Elab . ReaderC $ \ ty ctx ->
    ty `ex` \ res ->
    let' ((ctx |- a ::: type') ::: type') $ \ a' ->
    (var res ::: ty)
    ===
    ((p, var a') >-> (\ x -> ctx |> x ::: var a' |- b x ::: type') ::: type')

deriving instance (Ord v, Show v, Prob v t, Err t) => Prob v (Elab v t t)

deriving instance Err t => Err (Elab v t t)


elab :: Elab v t b ::: t -> Elab v t b
elab (Elab m ::: t) = Elab (local (const t) m)

typeOf :: (Ord v, Show v, Err t) => v -> Elab v t t
typeOf n = Elab (asks (!? n) >>= maybe (err ("free variable: " <> show n)) pure)

(|-) :: Map v t -> Elab v t t ::: t -> t
ctx |- b ::: t = runElab (ctx :|-: b ::: t)

infixl 1 |-

(|>) :: Ord v => Map v t -> v ::: t -> Map v t
ctx |> v ::: t = insert v t ctx

infixl 1 |>

check :: Prob v t => (Elab v t t ::: Elab v t t -> Elab v t t) -> Elab v t t
check f = Elab $ ask `ex` runElabC . \ v -> f (pure (var v) ::: Elab ask)


runScript :: (a -> t) -> Script t a -> t
runScript k (Script r) = r k

newtype Script t a = Script ((a -> t) -> t)
  deriving (Functor)

instance Applicative (Script t) where
  pure = Script . flip ($)
  (<*>) = ap

instance Monad (Script t) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)

meta :: Prob v t => t -> Script t v
meta = Script . ex

intro :: Lam v t => Script t v
intro = Script (lam Ex)

letbind :: Let v t => t ::: t -> Script t v
letbind = Script . let'
