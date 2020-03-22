{-# LANGUAGE TypeOperators #-}
module Tile.Library
( -- * Booleans
  baseBool
, bool
, true
, false
  -- * Functions
, baseFunction
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
  -- * Pair
, pair
, pair'
, fst
, snd
  -- * Nat
, nat
, z
, s
  -- * List
, list
, nil
, cons
  -- * Fin
, fin
, fz
, fs
) where

import Prelude hiding (either, fst, maybe, snd)
import Tile.Script
import Tile.Syntax

class Record expr where
  record :: [expr] -> expr


-- Booleans

baseBool :: (Module decl m, Def expr decl, Export decl, Lam expr, Record expr, Type expr) => m expr
baseBool = module' "Base.Bool" . runScript export $ do
  bool <- "Bool"
    .: type'
    := type' ==> \ _A -> _A --> _A --> _A

  false <- "False"
    .: bool
    := lam Ex (const (lam Ex id))

  true <- "True"
    .: bool
    := lam Ex (lam Ex . const)

  pure (record
    [ bool
    , false
    , true
    ])

bool :: Type expr => expr ::: expr
bool = (type' ==> \ _A -> _A --> _A --> _A) ::: type'

true :: (Lam expr, Type expr) => expr ::: expr
true = lam Ex (lam Ex . const) ::: tm bool

false :: (Lam expr, Type expr) => expr ::: expr
false = lam Ex (const (lam Ex id)) ::: tm bool


-- Functions

baseFunction :: (Module decl m, Def expr decl, Export decl, Lam expr, Let expr, Record expr, Type expr) => m expr
baseFunction = module' "Base.Function" . runScript export $ do
  id <- "id"
    .: type' ==> (\ _A -> _A --> _A)
    := lam Ex id

  const <- "const"
    .: type' ==> (\ _A -> type' ==> \ _B -> _A --> _B --> _A)
    := lam Ex (lam Ex . const)

  fix <- "fix"
    .: type' ==> (\ _A -> type' ==> \ _B -> ((_A --> _B) --> (_A --> _B)) --> (_A --> _B))
    := lam Im (\ _A -> lam Im (\ _B -> lam Ex (\ f -> let' (tm fix $$ f ::: _A --> _B) (\ fixf -> lam Ex (\ a -> f $$ fixf $$ a)))))

  pure (record
    [ id
    , const
    , fix
    ])

id' :: (Lam expr, Type expr) => expr ::: expr
id' = lam Ex id ::: type' ==> \ _A -> _A --> _A

const' :: (Lam expr, Type expr) => expr ::: expr
const' = lam Ex (lam Ex . const) ::: type' ==> \ _A -> type' ==> \ _B -> _A --> _B --> _A

fix :: (Lam expr, Let expr, Type expr) => expr ::: expr
fix = lam Im (\ _A -> lam Im (\ _B -> lam Ex (\ f -> let' (tm fix $$ f ::: _A --> _B) (\ fixf -> lam Ex (\ a -> f $$ fixf $$ a))))) ::: type' ==> \ _A -> type' ==> \ _B -> ((_A --> _B) --> (_A --> _B)) --> (_A --> _B)


-- Maybe

maybe :: (Lam expr, Type expr) => expr ::: expr
maybe = lam Ex (\ _A -> type' ==> \ _R -> _R --> (_A --> _R) --> _R) ::: type' --> type'

nothing :: (Lam expr, Type expr) => expr ::: expr
nothing = lam Ex (lam Ex . const) ::: type' ==> \ _A -> tm maybe $$ _A

just :: (Lam expr, Type expr) => expr ::: expr
just = lam Ex (\ a -> lam Ex (const (lam Ex ($$ a)))) ::: type' ==> \ _A -> _A --> tm maybe $$ _A


-- Either

either :: (Lam expr, Type expr) => expr ::: expr
either = lam Ex (\ _L -> lam Ex (\ _R -> type' ==> \ _K -> (_L --> _K) --> (_R --> _K) --> _K)) ::: type' --> type' --> type'

left :: (Lam expr, Type expr) => expr ::: expr
left = lam Ex (\ l -> lam Ex (\ left -> lam Ex (const (left $$ l)))) ::: type' ==> \ _L -> type' ==> \ _R -> _L --> tm either $$ _L $$ _R

right :: (Lam expr, Type expr) => expr ::: expr
right = lam Ex (\ r -> lam Ex (const (lam Ex ($$ r)))) ::: type' ==> \ _L -> type' ==> \ _R -> _R --> tm either $$ _L $$ _R


-- Pair

pair :: (Lam expr, Type expr) => expr ::: expr
pair = lam Ex (\ _L -> lam Ex (\ _R -> type' ==> \ _K -> (_L --> _R --> _K) --> _K)) ::: type' --> type' --> type'

pair' :: (Lam expr, Type expr) => expr ::: expr
pair' = lam Ex (\ l -> lam Ex (\ r -> lam Ex (\ k -> k $$ l $$ r))) ::: type' ==> \ _L -> type' ==> \ _R -> _L --> _R --> tm pair $$ _L $$ _R

fst :: (Lam expr, Type expr) => expr ::: expr
fst = lam Ex ($$ lam Ex (lam Ex . const)) ::: type' ==> \ _L -> type' ==> \ _R -> tm pair $$ _L $$ _R --> _L

snd :: (Lam expr, Type expr) => expr ::: expr
snd = lam Ex ($$ lam Ex (const (lam Ex id))) ::: type' ==> \ _L -> type' ==> \ _R -> tm pair $$ _L $$ _R --> _R


-- Nat

nat :: Type expr => expr ::: expr
nat = (type' ==> \ _R -> _R --> (_R --> _R) --> _R) ::: type'

z :: (Lam expr, Type expr) => expr ::: expr
z = lam Ex (lam Ex . const) ::: tm nat

s :: (Lam expr, Type expr) => expr ::: expr
s = lam Ex (\ x -> lam Ex (const (lam Ex ($$ x)))) ::: tm nat --> tm nat


-- List

list :: (Lam expr, Type expr) => expr ::: expr
list = lam Ex (\ _A -> type' ==> \ _R -> _R --> (_A --> _R --> _R) --> _R) ::: type' --> type'

nil :: (Lam expr, Type expr) => expr ::: expr
nil = lam Ex (lam Ex . const) ::: type' ==> \ _A -> tm list $$ _A

cons :: (Lam expr, Type expr) => expr ::: expr
cons = lam Ex (\ a -> lam Ex (\ as -> lam Ex (const (lam Ex (\ cons -> cons $$ a $$ as))))) ::: type' ==> \ _A -> tm list $$ _A


-- Fin

fin :: (Lam expr, Type expr) => expr ::: expr
fin = lam Ex (\ n -> (tm nat --> type') ==> \ _R -> _R $$ (tm s $$ n) --> (tm nat ==> \ n -> _R $$ n --> _R $$ (tm s $$ n)) --> _R $$ (tm s $$ n)) ::: tm nat --> type'

fz :: (Lam expr, Type expr) => expr ::: expr
fz = lam Ex (lam Ex . const) ::: tm nat ==> \ n -> tm fin $$ (tm s $$ n)

fs :: (Lam expr, Type expr) => expr ::: expr
fs = lam Ex (\ n -> lam Ex (const (lam Ex ($$ n)))) ::: tm nat ==> \ n -> tm fin $$ n --> tm fin $$ (tm s $$ n)


-- TODO: vectors
-- TODO: scott-encodings
