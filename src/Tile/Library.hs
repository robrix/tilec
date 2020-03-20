module Tile.Library
( -- * Booleans
  bool
, true
, false
  -- * Maybe
, maybe
, nothing
, just
  -- * Either
, either
  -- * Nat
, nat
, z
, s
) where

import Prelude hiding (either, maybe)
import Tile.Syntax

bool :: Type v expr => expr
bool = (Im, type') >-> \ a -> var a --> var a --> var a

true :: Lam v expr => expr
true = lam Ex (lam Ex . const . var)

false :: Lam v expr => expr
false = lam Ex (const (lam Ex var))


maybe :: Type v expr => expr
maybe = (Ex, type') >-> \ a -> (Im, type') >-> \ r -> var r --> (var a --> var r) --> var r

nothing :: Lam v expr => expr
nothing = lam Ex (lam Ex . const . var)

just :: Lam v expr => expr
just = lam Ex (\ a -> lam Ex (const (lam Ex (\ just -> var just $$ var a))))


either :: Type v expr => expr
either = (Ex, type') >-> \ l -> (Ex, type') >-> \ r -> (Im, type') >-> \ k -> (var l --> var k) --> (var r --> var k) --> var k


nat :: Type v expr => expr
nat = (Im, type') >-> \ r -> var r --> (var r --> var r) --> var r

z :: Lam v expr => expr
z = lam Ex (lam Ex . const . var)

s :: Lam v expr => expr
s = lam Ex (\ x -> lam Ex (const (lam Ex (\ s -> var s $$ var x))))
