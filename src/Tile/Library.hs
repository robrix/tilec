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

bool :: Type a expr => expr
bool = type' `pi'` \ a -> var a --> var a --> var a

true :: Lam a expr => expr
true = lam (lam . const . var)

false :: Lam a expr => expr
false = lam (const (lam var))


maybe :: Type a expr => expr
maybe = type' `pi'` \ a -> type' `pi'` \ r -> var r --> (var a --> var r) --> var r

nothing :: Lam a expr => expr
nothing = lam (lam . const . var)

just :: Lam a expr => expr
just = lam (const (lam var))


nat :: Type a expr => expr
nat = type' `pi'` \ r -> var r --> (var r --> var r) --> var r

z :: Lam a expr => expr
z = lam (lam . const . var)

s :: Lam a expr => expr
s = lam (\ x -> lam (const (lam (\ s -> var s $$ var x))))
