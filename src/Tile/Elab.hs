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
import Data.Functor.Identity
import Data.Map
import Tile.Syntax

runElab :: Elab v t t ::: t -> t
runElab (Elab m ::: t) = run (runReader empty (runReader t m))

newtype Elab v t a = Elab (ReaderC t (ReaderC (Map v t) Identity) a)
  deriving (Applicative, Functor, Monad)

instance (Ord v, Show v, Prob v t, Err t) => Var v (Elab v t t) where
  var n = check (=== (pure (var n) ::: typeOf n))

deriving instance (Ord v, Show v, Let v t, Prob v t, Err t) => Let v (Elab v t t)

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam p b = check $ \ exp ->
    type' `ex` \ _A ->
    (var _A --> type') `ex` \ _B ->
    exp === (lam p (\ x -> x ::: var _A |- elab (b x ::: var _B $$ var x)) ::: ((p, var _A) >-> \ x -> var _B $$ var x))

  f $$ a = check $ \ exp ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    exp === (($$) <$> elab (f ::: (var _A --> var _B)) <*> elab (a ::: var _A) ::: var _B)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = check (=== (pure type' ::: pure type'))

  (p, a) >-> b = check $ \ exp ->
    let' (elab (a ::: type') ::: pure type') $ \ a' ->
    exp === (((p, var a') >-> \ x -> x ::: var a' |- elab (b x ::: type')) ::: pure type')

deriving instance (Ord v, Show v, Prob v t, Err t) => Prob v (Elab v t t)

deriving instance Err t => Err (Elab v t t)


elab :: Elab v t b ::: t -> Elab v t b
elab (Elab m ::: t) = Elab (local (const t) m)

typeOf :: (Ord v, Show v, Err t) => v -> Elab v t t
typeOf n = Elab (asks (!? n) >>= maybe (err ("free variable: " <> show n)) pure)

(|-) :: Ord v => v ::: Elab v t t -> Elab v t t -> Elab v t t
(a ::: Elab t) |- Elab b = Elab $ do
  t' <- t
  local (insert a t') b

infixl 0 |-

check :: Prob v t => (Elab v t t ::: Elab v t t -> Elab v t t) -> Elab v t t
check f = Elab $ ask `ex` \ v -> let Elab m = f (pure (var v) ::: Elab ask) in m


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
