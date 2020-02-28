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

import Data.Maybe (fromMaybe)
import Tile.Stack
import Tile.Syntax
import Tile.Type

elab :: Stack t -> Elab t t -> t
elab = flip runElab

newtype Elab t b = Elab { runElab :: Stack t -> b }
  deriving (Applicative, Functor, Monad)

instance (Prob Int t, Type Int t, Err t) => Var Int (Elab t t) where
  var n = Elab $
    type' `ex` \ _A ->
    var _A `ex` \ v ->
    (var n ::: typeOf n)
    ===
    (var v ::: var _A)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (Elab t t) where
  let' tm b = Elab $
    type' `ex` \ _A ->
    let' (runElab tm .:. var _A) (var _A |- runElab . b)

instance (Lam Int t, Prob Int t, Type Int t, Err t) => Lam Int (Elab t t) where
  lam b = Elab $
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    lam (var _A |- runElab . b) .:. (var _A --> var _B)

  f $$ a = Elab $
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    var _B `ex` \ res ->
    let a' = runElab a in
    (runElab f $$ a' ::: (var _A --> var _B) $$ a')
    ===
    (var res ::: var _B)

instance (Prob Int t, Type Int t, Err t) => Type Int (Elab t t) where
  type' = Elab type'

  t >-> b = Elab $
    let t' = runElab t
    in (t' .:. type') >-> t' |- runElab . b

  tm .:. ty = Elab (runElab tm .:. runElab ty .:. type')

-- FIXME: this should likely have a Prob instance

typeOf :: Err t => Int -> Stack t -> t
typeOf n = fromMaybe (err ("free variable: " <> show n)) . (!? n)

(|-) :: (Stack t -> t) -> (a -> Stack t -> t) -> (a -> Stack t -> t)
(t |- b) a ctx = b a (ctx :> t ctx)

infixr 1 |-
