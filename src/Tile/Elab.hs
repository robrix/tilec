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
import Tile.Context
import Tile.Syntax

runElab :: Map v t :|-: Elab v t t ::: t -> t
runElab (ctx :|-: Elab m ::: t) = runReader t m ctx

newtype Elab v t a = Elab { runElabC :: ReaderC t ((->) (Map v t)) a }
  deriving (Applicative, Functor)

instance (Ord v, Show v, Prob v t, Err t) => Var v (Elab v t t) where
  var n = check (=== (pure (var n) ::: typeOf n))

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Let v (Elab v t t) where
  let' (tm ::: ty) b = check $ \ exp ->
    pure type' `ex` \ _A ->
    pure type' `ex` \ _B ->
    let ty' = elab (ty ::: type')
        tm' = elab (tm ::: var _A)
    in exp === (Elab (let' (runElabC tm' ::: runElabC ty') (\ x -> runElabC (x ::: var _A |- elab (b x ::: var _B)))) ::: pure (var _B))

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam p b = check $ \ exp ->
    pure type' `ex` \ _A ->
    pure (var _A --> type') `ex` \ _B ->
    exp === (Elab (lam p (\ x -> runElabC (x ::: var _A |- elab (b x ::: var _B $$ var x)))) ::: pure ((p, var _A) >-> \ x -> var _B $$ var x))

  f $$ a = Elab . ReaderC $ \ ty ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    ty `ex` \ res ->
    (var res ::: ty)
    ===
    (runElab (ctx :|-: f ::: var _A --> var _B) $$ runElab (ctx :|-: a ::: var _A) ::: var _B)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = pure type'

  (p, a) >-> b = check $ \ exp ->
    let' (elab (a ::: type') ::: pure type') $ \ a' ->
    exp === (Elab ((p, var a') >-> \ x -> runElabC (x ::: var a' |- elab (b x ::: type'))) ::: pure type')

deriving instance (Ord v, Show v, Prob v t, Err t) => Prob v (Elab v t t)

deriving instance Err t => Err (Elab v t t)


elab :: Elab v t b ::: t -> Elab v t b
elab (Elab m ::: t) = Elab (local (const t) m)

typeOf :: (Ord v, Show v, Err t) => v -> Elab v t t
typeOf n = Elab (asks (!? n) >>= maybe (err ("free variable: " <> show n)) pure)

(|-) :: Ord v => v ::: t -> Elab v t t -> Elab v t t
(a ::: t) |- Elab b = Elab (local (insert a t) b)

infixl 1 |-

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
