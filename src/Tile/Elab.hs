{-# LANGUAGE FlexibleContexts #-}
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
( elab
, Elab(..)
) where

import Data.Maybe (fromMaybe)
import Tile.Stack
import Tile.Syntax

elab :: Stack t -> Elab t -> t
elab = flip runElab

newtype Elab t = Elab { runElab :: Stack t -> t }

instance (Type Int t, Err t) => Var Int (Elab t) where
  var n = Elab $ \ ctx ->
    var n .:. fromMaybe (err ("free variable: " <> show n)) (ctx !? n)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (Elab t) where
  let' tm b = Elab $ \ ctx ->
    type' `ex` \ _A ->
    let' (elab ctx tm .:. var _A) (elab (ctx :> var _A) . b)

instance (Lam Int t, Prob Int t, Type Int t, Err t) => Lam Int (Elab t) where
  lam b = Elab $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (elab (ctx :> var _A) . b) .:. (var _A --> var _B)
  f $$ a = Elab $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    let f' = elab ctx f
        a' = elab ctx a
        _F = var _A --> var _B
    in (f' .:. _F) $$ (a' .:. var _A) .:. (_F $$ a' === var _B)

instance (Prob Int t, Type Int t, Err t) => Type Int (Elab t) where
  type' = Elab (const type')
  pi' t b = Elab $ \ ctx ->
    let t' = elab ctx t
    in pi' (t' .:. type') (elab (ctx :> t') . b)
  tm .:. ty = Elab $ \ ctx ->
    let ty' = elab ctx ty
        tm' = elab ctx tm
    in (tm' .:. ty' .:. type')

-- FIXME: this should likely have a Prob instance
