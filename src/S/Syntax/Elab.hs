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
module S.Syntax.Elab
( ElabC(..)
) where

import Data.Maybe (fromMaybe)
import S.Syntax
import S.Syntax.Classes

newtype ElabC t = ElabC { runElabC :: Stack t -> t ::: t }

instance (Var Int t, Err t) => Var Int (ElabC t) where
  var n = ElabC $ \ ctx ->
    var n ::: fromMaybe (err ("free variable: " <> show n)) (ctx !? n)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (ElabC t) where
  let' (tm ::: ty) b = ElabC $ \ ctx ->
    let ty' ::: tty' = elab ctx ty
        tm' ::: ttm' = elab ctx tm
    in let' ((tm' ::: ttm' === ty') ::: (ty' ::: tty' === type')) (elab (ctx :> ty') . b)

instance (Lam Int t, Prob Int t, Type Int t, Err t) => Lam Int (ElabC t) where
  lam b = ElabC $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (term_ . elab (ctx :> var _A) . b) ::: (var _A `pi'` const (var _B))
  f $$ a = ElabC $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    let f' ::: tf' = elab ctx f
        a' ::: ta' = elab ctx a
        _F = (ta' === var _A) `pi'` const (var _B)
    in f' $$ a' ::: (tf' === _F) $$ a'

instance (Prob Int t, Type Int t, Err t) => Type Int (ElabC t) where
  type' = ElabC (const type')
  pi' t b = ElabC $ \ ctx ->
    let t' ::: tt' = elab ctx t
    in pi' (t' ::: tt' === type') (elab (ctx :> t') . b)

-- FIXME: this should likely have a Prob instance

elab :: Stack t -> ElabC t -> t ::: t
elab = flip runElabC
