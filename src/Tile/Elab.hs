{-# LANGUAGE TypeOperators #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( elab
, Elab(..)
) where

import Tile.Script
import Tile.Syntax

elab :: Elab t ::: t -> t
elab (Elab run ::: ty) = run ty

newtype Elab t = Elab (t -> t)

var :: t -> Elab t
var = Elab . const

instance (Let t, Prob t, Type t) => Let (Elab t) where
  let' (v ::: t) b = check $ do
    _B <- meta type'
    t' <- letbind (elab (t ::: type') ::: type')
    pure
      (   let' (elab (v ::: t') ::: t') (\ x ->
            elab (b (var x) ::: _B))
      ::: _B)

instance (Lam t, Prob t, Type t) => Lam (Elab t) where
  lam b = check $ do
    _A <- meta type'
    _B <- meta (_A --> type')
    pure
      (   lam (\ x -> elab (b (var x) ::: _B $$ x))
      ::: _A ->> \ x -> _B $$ x)

  ilam b = check $ do
    _A <- meta type'
    _B <- meta (_A --> type')
    pure
      (   ilam (\ x -> elab (b (var x) ::: _B $$ x))
      ::: _A =>> \ x -> _B $$ x)

  ($$)  = app ($$)
  ($$?) = app ($$?)

app :: (Prob t, Type t) => (t -> t -> t) -> Elab t -> Elab t -> Elab t
app app f a = check $ do
  _A <- meta type'
  _B <- meta type'
  pure
    (   elab (f ::: _A --> _B) `app` elab (a ::: _A)
    ::: _B)

instance (Let t, Prob t, Type t) => Type (Elab t) where
  type' = check (pure (type' ::: type'))

  a ->> b = check $ do
    a' <- letbind (elab (a ::: type') ::: type')
    pure
      (   a' ->> (\ x -> elab (b (var x) ::: type'))
      ::: type')

  a =>> b = check $ do
    a' <- letbind (elab (a ::: type') ::: type')
    pure
      (   a' =>> (\ x -> elab (b (var x) ::: type'))
      ::: type')

instance (Let t, Prob t, Type t) => Prob (Elab t) where
  t `ex` b = check $ do
    _B <- meta type'
    t' <- letbind (elab (t ::: type') ::: type')
    pure
      (   t' `ex` (\ x -> elab (b (var x) ::: _B))
      ::: _B)

  (m1 ::: t1) === (m2 ::: t2) = check $ do
    t1' <- letbind (elab (t1 ::: type') ::: type')
    t2' <- letbind (elab (t2 ::: type') ::: type')
    pure
      (   (   elab (m1 ::: t1') ::: t1'
          === elab (m2 ::: t2') ::: t2')
      ::: (   t1' ::: type'
          === t2' ::: type'))


check :: Prob t => Script t (t ::: t) -> Elab t
check f = Elab $ \ ty -> evalScript $ do
  exp <- meta ty
  act <- f
  pure $! exp ::: ty === act
