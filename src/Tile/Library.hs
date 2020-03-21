{-# LANGUAGE TypeOperators #-}
module Tile.Library
( -- * Booleans
  bool
, true
, false
  -- * Functions
, id'
, const'
, fix
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
  -- * List
, list
, nil
, cons
) where

import Prelude hiding (either, maybe)
import Tile.Syntax
import Tile.Type

-- Booleans

bool :: Type expr => expr ::: expr
bool = ((Im, type') >-> \ _A -> _A --> _A --> _A) ::: type'

true :: (Lam expr, Type expr) => expr ::: expr
true = lam Ex (lam Ex . const) ::: tm bool

false :: (Lam expr, Type expr) => expr ::: expr
false = lam Ex (const (lam Ex id)) ::: tm bool


-- Functions

id' :: (Lam expr, Type expr) => expr ::: expr
id' = lam Ex id ::: (Im, type') >-> \ _A -> _A --> _A

const' :: (Lam expr, Type expr) => expr ::: expr
const' = lam Ex (lam Ex . const) ::: (Im, type') >-> \ _A -> (Im, type') >-> \ _B -> _A --> _B --> _A

fix :: (Lam expr, Let expr, Type expr) => expr ::: expr
fix = lam Im (\ _A -> lam Im (\ _B -> lam Ex (\ f -> let' (tm fix $$ f ::: _A --> _B) (\ fixf -> lam Ex (\ a -> f $$ fixf $$ a))))) ::: type' ==> \ _A -> type' ==> \ _B -> ((_A --> _B) --> (_A --> _B)) --> (_A --> _B)


-- Maybe

maybe :: (Lam expr, Type expr) => expr ::: expr
maybe = lam Ex (\ _A -> (Im, type') >-> \ _R -> _R --> (_A --> _R) --> _R) ::: type' --> type'

nothing :: (Lam expr, Type expr) => expr ::: expr
nothing = lam Ex (lam Ex . const) ::: (Im, type') >-> \ _A -> tm maybe $$ _A

just :: (Lam expr, Type expr) => expr ::: expr
just = lam Ex (\ a -> lam Ex (const (lam Ex ($$ a)))) ::: (Im, type') >-> \ _A -> _A --> tm maybe $$ _A


-- Either

either :: (Lam expr, Type expr) => expr ::: expr
either = lam Ex (\ _L -> lam Ex (\ _R -> (Im, type') >-> \ _K -> (_L --> _K) --> (_R --> _K) --> _K)) ::: type' --> type' --> type'

left :: (Lam expr, Type expr) => expr ::: expr
left = lam Ex (\ l -> lam Ex (\ left -> lam Ex (const (left $$ l)))) ::: (Im, type') >-> \ _L -> (Im, type') >-> \ _R -> _L --> tm either $$ _L $$ _R

right :: (Lam expr, Type expr) => expr ::: expr
right = lam Ex (\ r -> lam Ex (const (lam Ex ($$ r)))) ::: (Im, type') >-> \ _L -> (Im, type') >-> \ _R -> _R --> tm either $$ _L $$ _R


-- Nat

nat :: Type expr => expr ::: expr
nat = ((Im, type') >-> \ _R -> _R --> (_R --> _R) --> _R) ::: type'

z :: (Lam expr, Type expr) => expr ::: expr
z = lam Ex (lam Ex . const) ::: tm nat

s :: (Lam expr, Type expr) => expr ::: expr
s = lam Ex (\ x -> lam Ex (const (lam Ex ($$ x)))) ::: tm nat --> tm nat


-- List

list :: (Lam expr, Type expr) => expr ::: expr
list = lam Ex (\ _A -> (Im, type') >-> \ _R -> _R --> (_A --> _R --> _R) --> _R) ::: type' --> type'

nil :: (Lam expr, Type expr) => expr ::: expr
nil = lam Ex (lam Ex . const) ::: (Im, type') >-> \ _A -> tm list $$ _A

cons :: (Lam expr, Type expr) => expr ::: expr
cons = lam Ex (\ a -> lam Ex (\ as -> lam Ex (const (lam Ex (\ cons -> cons $$ a $$ as))))) ::: (Im, type') >-> \ _A -> tm list $$ _A


-- TODO: vectors
-- TODO: finite sets
-- TODO: scott-encodings
