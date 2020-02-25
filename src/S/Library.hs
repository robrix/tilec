module S.Library
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
import S.Syntax.Classes

bool :: Type a expr => expr a
bool = type' `pi'` \ a -> var a --> var a --> var a

true :: Lam a expr => expr a
true = lam (lam . const . var)

false :: Lam a expr => expr a
false = lam (const (lam var))


maybe :: Type a expr => expr a
maybe = type' `pi'` \ a -> type' `pi'` \ r -> var r --> (var a --> var r) --> var r

nothing :: Lam a expr => expr a
nothing = lam (lam . const . var)

just :: Lam a expr => expr a
just = lam (const (lam var))


nat :: Type a expr => expr a
nat = type' `pi'` \ r -> var r --> (var r --> var r) --> var r

z :: Lam a expr => expr a
z = lam (lam . const . var)

s :: Lam a expr => expr a
s = lam (\ x -> lam (const (lam (\ s -> var s $$ var x))))
