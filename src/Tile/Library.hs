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
    := type' =>> \ _A -> _A --> _A --> _A

  false <- "False"
    .: bool
    := lams (\ _ b -> Return b)

  true <- "True"
    .: bool
    := lams (\ a _ -> Return a)

  pure (record
    [ bool
    , false
    , true
    ])

bool :: Type expr => expr ::: expr
bool = (type' =>> \ _A -> _A --> _A --> _A) ::: type'

false :: (Lam expr, Type expr) => expr ::: expr
false = lams (\ _ b -> Return b) ::: tm bool

true :: (Lam expr, Type expr) => expr ::: expr
true = lams (\ a _ -> Return a) ::: tm bool


-- Functions

baseFunction :: (Module decl m, Def expr decl, Export decl, Lam expr, Let expr, Record expr, Type expr) => m expr
baseFunction = module' "Base.Function" . runScript export $ do
  id <- "id"
    .: type' =>> (\ _A -> _A --> _A)
    := lams Return

  const <- "const"
    .: type' *=>> (\ _A _B -> Return $ _A --> _B --> _A)
    := lams (\ a _ -> Return a)

  fix <- "fix"
    .: type' *=>> (\ _A _B -> Return $ ((_A --> _B) --> (_A --> _B)) --> (_A --> _B))
    := ilams (\ _A _B -> Return $ lam (\ f -> let' (tm fix $$ f ::: _A --> _B) (\ fixf -> lam (\ a -> f $$ fixf $$ a))))

  pure (record
    [ id
    , const
    , fix
    ])

id' :: (Lam expr, Type expr) => expr ::: expr
id'
  =   lams Return
  ::: type' =>> \ _A -> _A --> _A

const' :: (Lam expr, Type expr) => expr ::: expr
const'
  =   lams (\ a _ -> Return a)
  ::: type' *=>> \ _A _B -> Return $ _A --> _B --> _A

fix :: (Lam expr, Let expr, Type expr) => expr ::: expr
fix
  =   ilams (\ _A _B -> Return $ lam (\ f -> let' (tm fix $$ f ::: _A --> _B) (\ fixf -> lam (\ a -> f $$ fixf $$ a))))
  ::: type' *=>> \ _A _B -> Return $ ((_A --> _B) --> (_A --> _B)) --> (_A --> _B)


-- Maybe

maybe :: (Lam expr, Type expr) => expr ::: expr
maybe
  =   lam (\ _A -> type' =>> \ _R -> _R --> (_A --> _R) --> _R)
  ::: type' --> type'

nothing :: (Lam expr, Type expr) => expr ::: expr
nothing
  =   lams (\ a _ -> Return a)
  ::: type' =>> \ _A -> tm maybe $$ _A

just :: (Lam expr, Type expr) => expr ::: expr
just
  =   lams (\ a _ just -> Return $ just $$ a)
  ::: type' =>> \ _A -> _A --> tm maybe $$ _A


-- Either

either :: (Lam expr, Type expr) => expr ::: expr
either
  =   lams (\ _L _R -> Return $ type' =>> \ _K -> (_L --> _K) --> (_R --> _K) --> _K)
  ::: type' --> type' --> type'

left :: (Lam expr, Type expr) => expr ::: expr
left
  =   lams (\ l left _ -> Return $ left $$ l)
  ::: type' *=>> \ _L _R -> Return $ _L --> tm either $$ _L $$ _R

right :: (Lam expr, Type expr) => expr ::: expr
right
  =   lams (\ r _ right -> Return $ right $$ r)
  ::: type' *=>> \ _L _R -> Return $ _R --> tm either $$ _L $$ _R


-- Pair

pair :: (Lam expr, Type expr) => expr ::: expr
pair
  =   lams (\ _L _R -> Return $ type' =>> \ _K -> (_L --> _R --> _K) --> _K)
  ::: type' --> type' --> type'

pair' :: (Lam expr, Type expr) => expr ::: expr
pair'
  =   lams (\ fst snd k -> Return $ k $$ fst $$ snd)
  ::: type' *=>> \ _L _R -> Return $ _L --> _R --> tm pair $$ _L $$ _R

fst :: (Lam expr, Type expr) => expr ::: expr
fst
  =   lam ($$ lams (\ fst _ -> Return fst))
  ::: type' *=>> \ _L _R -> Return $ tm pair $$ _L $$ _R --> _L

snd :: (Lam expr, Type expr) => expr ::: expr
snd
  =   lam ($$ lams (\ _ snd -> Return snd))
  ::: type' *=>> \ _L _R -> Return $ tm pair $$ _L $$ _R --> _R


-- Nat

nat :: Type expr => expr ::: expr
nat
  =   type' =>> (\ _R -> _R --> (_R --> _R) --> _R)
  ::: type'

z :: (Lam expr, Type expr) => expr ::: expr
z
  =   lams (\ z _ -> Return z)
  ::: tm nat

s :: (Lam expr, Type expr) => expr ::: expr
s
  =   lams (\ x _ s -> Return $ s $$ x)
  ::: tm nat --> tm nat


-- List

list :: (Lam expr, Type expr) => expr ::: expr
list
  =   lam (\ _A -> type' =>> \ _R -> _R --> (_A --> _R --> _R) --> _R)
  ::: type' --> type'

nil :: (Lam expr, Type expr) => expr ::: expr
nil
  =   lams (\ nil _ -> Return nil)
  ::: type' =>> \ _A -> tm list $$ _A

cons :: (Lam expr, Type expr) => expr ::: expr
cons
  =   lams (\ a as _ cons -> Return $ cons $$ a $$ as)
  ::: type' =>> \ _A -> tm list $$ _A


-- Fin

fin :: (Lam expr, Type expr) => expr ::: expr
fin
  =   lam (\ n -> (tm nat --> type') =>> \ _R -> _R $$ (tm s $$ n) --> (tm nat =>> \ n -> _R $$ n --> _R $$ (tm s $$ n)) --> _R $$ (tm s $$ n))
  ::: tm nat --> type'

fz :: (Lam expr, Type expr) => expr ::: expr
fz
  =   lams (\ fz _ -> Return fz)
  ::: tm nat =>> \ n -> tm fin $$ (tm s $$ n)

fs :: (Lam expr, Type expr) => expr ::: expr
fs
  =   lams (\ n _ fs -> Return $ fs $$ n)
  ::: tm nat =>> \ n -> tm fin $$ n --> tm fin $$ (tm s $$ n)


-- TODO: vectors
-- TODO: scott-encodings
