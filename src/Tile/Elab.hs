{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Elaboration, implemented as a mash-up of:
--
-- * [An Algebraic Approach to Typechecking and Elaboration](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html), Bob Atkey
-- * Typed Tagless Final Interpreters, Oleg Kiselyov
-- * Type checking through unification, Francesco Mazzoli, Andreas Abel
module Tile.Elab
( Elab(..)
, Script(..)
) where

import Control.Carrier.Reader
import Data.Functor.Identity
import Data.Map
import Tile.Syntax

newtype Elab v t b = Elab { runElab :: ReaderC t (ReaderC (Map v t) Identity) b }
  deriving (Applicative, Functor, Monad)

instance (Ord v, Show v, Prob v t, Err t) => Var v (Elab v t t) where
  var n = check (=== (var n ::: typeOf n))

deriving instance (Ord v, Show v, Let v t, Prob v t, Err t) => Let v (Elab v t t)

instance (Ord v, Show v, Let v t, Lam v t, Prob v t, Type v t, Err t) => Lam v (Elab v t t) where
  lam p b = check $ \ exp ->
    type' `ex` \ _A ->
    (var _A --> type') `ex` \ _B ->
    exp === (lam p (\ x -> x ::: var _A |- elab (b x ::: var _B $$ var x)) ::: ((p, var _A) >-> \ x -> var _B $$ var x))

  f $$ a = check $ \ exp ->
    type' `ex` \ _A ->
    type' `ex` \ _B ->
    exp === (elab (f ::: (var _A --> var _B)) $$ elab (a ::: var _A) ::: var _B)

instance (Ord v, Show v, Let v t, Prob v t, Type v t, Err t) => Type v (Elab v t t) where
  type' = check (=== (type' ::: type'))

  (p, a) >-> b = check $ \ exp ->
    let' (elab (a ::: type')) $ \ a' ->
    exp === (((p, var a') >-> \ x -> x ::: var a' |- elab (b x ::: type')) ::: type')

deriving instance (Ord v, Show v, Prob v t, Err t) => Prob v (Elab v t t)

deriving instance Err t => Err (Elab v t t)


elab :: Elab v t b ::: t -> Elab v t b
elab (m ::: t) = Elab (local (const t) (runElab m))

typeOf :: (Ord v, Show v, Err t) => v -> Elab v t t
typeOf n = Elab (asks (!? n) >>= maybe (err ("free variable: " <> show n)) pure)

(|-) :: Ord v => v ::: Elab v t t -> Elab v t t -> Elab v t t
(a ::: t) |- b = Elab $ do
  t' <- runElab t
  local (insert a t') (runElab b)

infixl 0 |-

check :: Prob v t => (Elab v t t ::: Elab v t t -> Elab v t t) -> Elab v t t
check f = Elab $ ask `ex` \ v -> runElab (f (pure (var v) ::: Elab ask))


newtype Script t a = Script ((a -> t) -> t)
