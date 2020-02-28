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

import Data.Map
import Data.Maybe (fromMaybe)
import Tile.Syntax
import Tile.Type

newtype Elab v t b = Elab { runElab :: Map v t -> b }
  deriving (Applicative, Functor, Monad)

instance (Ord v, Show v, Prob v t, Type v t, Err t) => Var v (Elab v t t) where
  var n = Elab $
    type' `ex` \ _A ->
    var _A `ex` \ v ->
    (var n ::: typeOf n)
    ===
    (var v ::: var _A)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Let v (Elab v t t) where
  let' tm b = Elab $
    type' `ex` \ _A ->
    let' (runElab tm .:. var _A) (var _A |- runElab . b)

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam b = Elab $
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (var _A |- runElab . b) .:. (var _A --> var _B)

  f $$ a = Elab $
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    var _B `ex` \ res ->
    let' (runElab a) $ \ a' ->
    (runElab f $$ var a' ::: (var _A --> var _B) $$ var a')
    ===
    (var res ::: var _B)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = Elab type'

  a >-> b = Elab .
    let' (runElab a) $ \ a' ->
    (var a' .:. type') >-> var a' |- runElab . b

  tm .:. ty = Elab (runElab tm .:. runElab ty .:. type')

-- FIXME: this should likely have a Prob instance

typeOf :: (Ord v, Show v) => Err t => v -> Map v t -> t
typeOf n = fromMaybe (err ("free variable: " <> show n)) . (!? n)

(|-) :: Ord v => (Map v t -> t) -> (v -> Map v t -> t) -> (v -> Map v t -> t)
(t |- b) a ctx = b a (insert a (t ctx) ctx)

infixr 1 |-
