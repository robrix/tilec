{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( Elab(..)
) where

import Control.Carrier.Reader
import Data.Functor.Identity
import Data.Map
import Tile.Syntax
import Tile.Type

elab :: Elab v t b ::: t -> ReaderC (Map v t) Identity b
elab (m ::: t) = runReader t (runElab m)

newtype Elab v t b = Elab { runElab :: ReaderC t (ReaderC (Map v t) Identity) b }
  deriving (Applicative, Functor, Monad)

instance (Ord v, Show v, Prob v t, Type v t, Err t) => Var v (Elab v t t) where
  var n = Elab . ReaderC $ \ t ->
    pure t `ex` \ v ->
    (var n ::: typeOf n)
    ===
    (var v ::: pure t)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Let v (Elab v t t) where
  let' tm b = Elab . ReaderC $ \ t ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    pure t `ex` \ v ->
    (let' (elab (tm ::: var _A)) (\ x -> x ::: var _A |- elab (b x ::: var _B)) ::: var _B)
    ===
    (var v ::: pure t)

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam b = Elab . ReaderC $ \ t ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    pure t `ex` \ v ->
    (lam (\ x -> x ::: var _A |- elab (b x ::: var _B)) ::: (var _A --> var _B))
    ===
    (var v ::: pure t)

  f $$ a = Elab . ReaderC $ \ t ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    pure t `ex` \ v ->
    let' (elab (a ::: var _A)) $ \ a' ->
    let' (var _A --> var _B) $ \ f' ->
    (elab (f ::: var f') $$ var a' ::: var f' $$ var a')
    ===
    (var v ::: pure t)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = Elab . ReaderC $ \ t ->
    pure t `ex` \ v ->
    (type' ::: type')
    ===
    (var v ::: pure t)

  a >-> b = Elab . ReaderC $ \ t ->
    type' `ex` \ _A ->
    (var _A --> type') `ex` \ _B ->
    pure t `ex` \ v ->
    let' (elab (a ::: type')) $ \ a' ->
    ((var a' >-> \ x -> x ::: var a' |- elab (b x ::: var _B)) ::: type')
    ===
    (var v ::: pure t)

-- FIXME: this should likely have a Prob instance

typeOf :: (Has (Reader (Map v t)) sig m, Ord v, Show v, Err (m t)) => v -> m t
typeOf n = asks (!? n) >>= maybe (err ("free variable: " <> show n)) pure

(|-) :: (Has (Reader (Map v t)) sig m, Ord v) => v ::: m t -> m t -> m t
(a ::: t) |- b = do
  t' <- t
  local (insert a t') b

infixl 0 |-
