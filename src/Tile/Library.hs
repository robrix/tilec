{-# LANGUAGE TypeOperators #-}
module Tile.Library
( -- * Booleans
  bool
, true
, false
  -- * Functions
, id'
, const'
  -- * Maybe
, maybe
, nothing
, just
  -- * Either
, either
, left
, right
  -- * Nat
, nat
, z
, s
) where

import Prelude hiding (either, maybe)
import Tile.Syntax
import Tile.Type

bool :: Type v expr => expr ::: expr
bool = ((Im, type') >-> \ _A -> var _A --> var _A --> var _A) ::: type'

true :: (Lam v expr, Type v expr) => expr ::: expr
true = lam Ex (lam Ex . const . var) ::: tm bool

false :: (Lam v expr, Type v expr) => expr ::: expr
false = lam Ex (const (lam Ex var)) ::: tm bool


id' :: (Lam v expr, Type v expr) => expr ::: expr
id' = lam Ex var ::: (Im, type') >-> \ _A -> var _A --> var _A

const' :: (Lam v expr, Type v expr) => expr ::: expr
const' = lam Ex (lam Ex . const . var) ::: (Im, type') >-> \ _A -> (Im, type') >-> \ _B -> var _A --> var _B --> var _A


maybe :: (Lam v expr, Type v expr) => expr ::: expr
maybe = lam Ex (\ _A -> (Im, type') >-> \ _R -> var _R --> (var _A --> var _R) --> var _R) ::: type' --> type'

nothing :: (Lam v expr, Type v expr) => expr ::: expr
nothing = lam Ex (lam Ex . const . var) ::: (Im, type') >-> \ _A -> tm maybe $$ var _A

just :: (Lam v expr, Type v expr) => expr ::: expr
just = lam Ex (\ a -> lam Ex (const (lam Ex (\ just -> var just $$ var a)))) ::: (Im, type') >-> \ _A -> var _A --> tm maybe $$ var _A


either :: (Lam v expr, Type v expr) => expr
either = lam Ex (\ _L -> lam Ex (\ _R -> (Im, type') >-> \ _K -> (var _L --> var _K) --> (var _R --> var _K) --> var _K))

left :: Lam v expr => expr
left = lam Ex (\ l -> lam Ex (\ left -> lam Ex (const (var left $$ var l))))

right :: Lam v expr => expr
right = lam Ex (\ r -> lam Ex (const (lam Ex (\ right -> var right $$ var r))))


nat :: Type v expr => expr
nat = (Im, type') >-> \ _R -> var _R --> (var _R --> var _R) --> var _R

z :: Lam v expr => expr
z = lam Ex (lam Ex . const . var)

s :: Lam v expr => expr
s = lam Ex (\ x -> lam Ex (const (lam Ex (\ s -> var s $$ var x))))
