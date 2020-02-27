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
import Tile.Type

elab :: Stack t -> Elab t -> t ::: t
elab = flip runElab

newtype Elab t = Elab { runElab :: Stack t -> t ::: t }

instance (Var Int t, Err t) => Var Int (Elab t) where
  var n = Elab $ \ ctx ->
    var n ::: fromMaybe (err ("free variable: " <> show n)) (ctx !? n)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (Elab t) where
  let' (tm ::: ty) b = Elab $ \ ctx ->
    let ty' ::: tty' = elab ctx ty
        tm' ::: ttm' = elab ctx tm
    -- FIXME: this is almost certainly wrong
    in let' ((tm' ::: ttm' === ty') ::: (ty' ::: tty' === type')) (elab (ctx :> ty') . b)

instance (Lam Int t, Prob Int t, Type Int t, Err t) => Lam Int (Elab t) where
  lam b = Elab $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (term_ . elab (ctx :> var _A) . b) ::: (var _A `pi'` const (var _B))
  f $$ a = Elab $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    let f' ::: tf' = elab ctx f
        a' ::: ta' = elab ctx a
        _F = (ta' === var _A) `pi'` const (var _B)
    in f' $$ a' ::: (tf' === _F) $$ a'

instance (Prob Int t, Type Int t, Err t) => Type Int (Elab t) where
  type' = Elab (const type')
  pi' t b = Elab $ \ ctx ->
    let t' ::: tt' = elab ctx t
    in pi' (t' ::: tt' === type') (elab (ctx :> t') . b)
  tm .:. ty = Elab $ \ ctx ->
    let ty' ::: tty' = elab ctx ty
        tm' ::: ttm' = elab ctx tm
    in (tm' ::: ttm' === ty') .:. (ty' ::: tty' === type')

-- FIXME: this should likely have a Prob instance
