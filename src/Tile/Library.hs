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

bool :: Type v a expr => expr a
bool = (Im, type') >-> \ a -> var a --> var a --> var a

true :: Lam v a expr => expr a
true = lam Ex (lam Ex . const . var)

false :: Lam v a expr => expr a
false = lam Ex (const (lam Ex var))


maybe :: Type v a expr => expr a
maybe = (Ex, type') >-> \ a -> (Im, type') >-> \ r -> var r --> (var a --> var r) --> var r

nothing :: Lam v a expr => expr a
nothing = lam Ex (lam Ex . const . var)

just :: Lam v a expr => expr a
just = lam Ex (\ a -> lam Ex (const (lam Ex (\ just -> var just $$ var a))))


nat :: Type v a expr => expr a
nat = (Im, type') >-> \ r -> var r --> (var r --> var r) --> var r

z :: Lam v a expr => expr a
z = lam Ex (lam Ex . const . var)

s :: Lam v a expr => expr a
s = lam Ex (\ x -> lam Ex (const (lam Ex (\ s -> var s $$ var x))))
