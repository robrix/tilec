module Tile.Library
( -- * Booleans
  bool
, true
, false
  -- * Maybe
, maybe
, nothing
, just
  -- * Nat
, nat
, z
, s
) where

import Prelude hiding (maybe)
import Tile.Syntax

bool :: Type v expr => expr
bool = (Im, type') >-> \ a -> var a --> var a --> var a

true :: Lam v expr => expr
true = lam (lam . const . var)

false :: Lam v expr => expr
false = lam (const (lam var))


maybe :: Type v expr => expr
maybe = (Ex, type') >-> \ a -> (Im, type') >-> \ r -> var r --> (var a --> var r) --> var r

nothing :: Lam v expr => expr
nothing = lam (lam . const . var)

just :: Lam v expr => expr
just = lam (const (lam var))


nat :: Type v expr => expr
nat = (Im, type') >-> \ r -> var r --> (var r --> var r) --> var r

z :: Lam v expr => expr
z = lam (lam . const . var)

s :: Lam v expr => expr
s = lam (\ x -> lam (const (lam (\ s -> var s $$ var x))))
