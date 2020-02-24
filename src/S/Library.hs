module S.Library
( -- * Booleans
  bool
, true
, false
  -- * Maybe
, maybe
) where

import Prelude hiding (maybe)
import S.Syntax.Classes

bool :: Type expr a => expr a
bool = type' `pi'` \ a -> var a --> var a --> var a

true :: Lam expr a => expr a
true = lam (lam . const . var)

false :: Lam expr a => expr a
false = lam (const (lam var))


maybe :: Type expr a => expr a
maybe = type' `pi'` \ a -> type' `pi'` \ r -> var r --> (var a --> var r) --> var r
