module S.Library
( -- * Booleans
  bool
, true
, false
) where

import S.Syntax.Classes

bool :: Type expr a => expr a
bool = type' `pi'` \ a -> var a --> var a --> var a

true :: Lam expr a => expr a
true = lam (lam . const . var)

false :: Lam expr a => expr a
false = lam (const (lam var))
