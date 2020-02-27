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
( elab
, Elab(..)
) where

import Control.Carrier.Reader
import Data.Maybe (fromMaybe)
import Tile.Stack
import Tile.Syntax
import Tile.Type

elab :: Stack (t a) -> Elab t a b -> t b
elab ctx = runReader ctx . runElab

newtype Elab t a b = Elab { runElab :: ReaderC (Stack (t a)) t b }
  deriving (Applicative, Functor, Monad)

instance (Prob Int t, Type Int t, Err t) => Var Int (Elab t Int) where
  var n = Elab . ReaderC $ \ ctx ->
    type' `ex` \ _A ->
    var _A `ex` \ v ->
    (var n ::: fromMaybe (err ("free variable: " <> show n)) (ctx !? n))
    ===
    (var v ::: var _A)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (Elab t Int) where
  let' tm b = Elab . ReaderC $ \ ctx ->
    type' `ex` \ _A ->
    let' (elab ctx tm .:. var _A) (elab (ctx :> var _A) . b)

instance (Lam Int t, Prob Int t, Type Int t, Err t) => Lam Int (Elab t Int) where
  lam b = Elab . ReaderC $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (elab (ctx :> var _A) . b) .:. (var _A --> var _B)

  f $$ a = Elab . ReaderC $ \ ctx ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    var _B `ex` \ res ->
    let f' = elab ctx f
        a' = elab ctx a
        _F = var _A --> var _B
    in
    (f' $$ a' ::: _F $$ a')
    ===
    (var res ::: var _B)

instance (Prob Int t, Type Int t, Err t) => Type Int (Elab t Int) where
  type' = Elab (ReaderC (const type'))

  t >-> b = Elab . ReaderC $ \ ctx ->
    let t' = elab ctx t
    in (t' .:. type') >-> elab (ctx :> t') . b

  tm .:. ty = Elab . ReaderC $ \ ctx ->
    let ty' = elab ctx ty
        tm' = elab ctx tm
    in tm' .:. ty' .:. type'

-- FIXME: this should likely have a Prob instance

typeOf :: Err t => Int -> ReaderC (Stack (t a)) t a
typeOf n = ReaderC $ fromMaybe (err ("free variable: " <> show n)) . (!? n)

(|-) :: t a -> Elab t a b -> Elab t a b
t |- b = Elab . ReaderC $ \ ctx -> runReader (ctx :> t) (runElab b)

infixr 1 |-
