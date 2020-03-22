{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( elab
, ElabC(ElabC)
) where

import Tile.Script
import Tile.Syntax

elab :: ElabC t t ::: t -> t
elab (ElabC run ::: ty) = run ty

newtype ElabC t a = ElabC (t -> a)

var :: a -> ElabC t a
var = ElabC . const

instance (Let t, Prob t, Type t) => Let (ElabC t t) where
  let' (v ::: t) b = check $ do
    _B <- meta type'
    t' <- letbind (elab (t ::: type') ::: type')
    pure
      (   let' (elab (v ::: t') ::: t') (\ x ->
            elab (b (var x) ::: _B))
      ::: _B)

instance (Lam t, Prob t, Type t) => Lam (ElabC t t) where
  lam p b = check $ do
    _A <- meta type'
    _B <- meta (_A --> type')
    pure
      (   lam p (\ x -> elab (b (var x) ::: _B $$ x))
      ::: (p, _A) >-> \ x -> _B $$ x)

  f $$ a = check $ do
    _A <- meta type'
    _B <- meta type'
    pure
      (   elab (f ::: _A --> _B) $$ elab (a ::: _A)
      ::: _B)

instance (Let t, Prob t, Type t) => Type (ElabC t t) where
  type' = check (pure (type' ::: type'))

  (p, a) >-> b = check $ do
    a' <- letbind (elab (a ::: type') ::: type')
    pure
      (   (p, a') >-> (\ x -> elab (b (var x) ::: type'))
      ::: type')

instance (Let t, Prob t, Type t) => Prob (ElabC t t) where
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


check :: Prob t => Script t (t ::: t) -> ElabC t t
check f = ElabC $ \ ty -> evalScript $ do
  exp <- meta ty
  act <- f
  pure $! exp ::: ty === act
