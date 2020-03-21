{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( elab
, ElabC(ElabC)
) where

import Tile.Syntax

elab :: ElabC a m a ::: m a -> m a
elab (ElabC run ::: ty) = run ty

newtype ElabC t m a = ElabC (m t -> m a)

var :: m a -> ElabC t m a
var = ElabC . const

instance (Let (m a), Prob (m a), Type (m a)) => Let (ElabC a m a) where
  let' (v ::: t) b = check $ do
    _B <- meta type'
    t' <- letbind (elab (t ::: type') ::: type')
    pure
      (   let' (elab (v ::: t') ::: t') (\ x ->
            elab (b (var x) ::: _B))
      ::: _B)

instance (Lam (m a), Prob (m a), Type (m a)) => Lam (ElabC a m a) where
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

instance (Let (m a), Prob (m a), Type (m a)) => Type (ElabC a m a) where
  type' = check (pure (type' ::: type'))

  (p, a) >-> b = check $ do
    a' <- letbind (elab (a ::: type') ::: type')
    pure
      (   (p, a') >-> (\ x -> elab (b (var x) ::: type'))
      ::: type')

instance (Let (m a), Prob (m a), Type (m a)) => Prob (ElabC a m a) where
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


check :: Prob (m a) => Script (m a) (m a ::: m a) -> ElabC a m a
check f = ElabC $ \ ty -> runScript id $ do
  exp <- meta ty
  act <- f
  pure $! exp ::: ty === act


newtype Err v
  = FreeVariable v
  deriving (Eq, Ord, Show)
