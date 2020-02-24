module S.Library
( -- * Booleans
  bool
) where

import S.Syntax.Classes

bool :: Type expr a => expr a
bool = type' `pi'` \ a -> var a --> var a --> var a

