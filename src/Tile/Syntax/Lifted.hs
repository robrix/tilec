{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax.Lifted
( Permutable
, S.Syntax
, var
, wvar
  -- * Let
, S.Let
, let'
, let''
  -- * Lam
, S.Lam
, lam
, ($$)
  -- * Type
, S.Type
, type'
, (>->)
, (-->)
, (==>)
  -- * Prob
, S.Prob
, ex
, (===)
  -- * Modules
, S.Def
  -- * Elaborator scripts
, runScript
, runCScript
, throw
, reset
, resetC
, liftScript
, liftCScript
, Script(..)
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import           Control.Applicative (liftA2)
import           Data.Distributive
import           Tile.Functor.Compose
import           Tile.Syntax ((:::)(..), Plicit(..), plicit)
import qualified Tile.Syntax as S

type Permutable f = (Applicative f, Distributive f)

var :: Applicative m => i expr -> m (i expr)
var = pure

wvar :: forall m i j expr . (Applicative m, Extends i j) => i expr -> m (j expr)
wvar = var @m . weakens


-- Let

let'
  :: (Applicative m, S.Let expr, Permutable i)
  => m (i expr) ::: m (i expr)
  -> (forall j . Permutable j => (i :.: j) expr -> m ((i :.: j) expr))
  -> m (i expr)
let' v f = let'' v (f . C . pure)

let''
  :: (Applicative m, S.Let expr, Permutable i)
  => m (i expr) ::: m (i expr)
  -> (forall j . Permutable j => j expr -> m ((i :.: j) expr))
  -> m (i expr)
let'' (tm ::: ty) f = liftA2 S.let' <$> (liftA2 (:::) <$> tm <*> ty) <*> (getC <$> f id)


-- Lam

lam :: (Applicative m, S.Lam expr, Permutable i) => m (i Plicit) -> (forall j . Permutable j => (i :.: j) expr -> m ((i :.: j) expr)) -> m (i expr)
lam p f = liftA2 S.lam <$> p <*> (getC <$> f (C (pure id)))

($$) :: (Applicative m, S.Lam expr) => m expr -> m expr -> m expr
($$) = liftA2 (S.$$)

infixl 9 $$


-- Type

type' :: (Applicative m, S.Type expr) => m expr
type' = pure S.type'

(>->) :: (Applicative m, S.Type expr, Permutable i) => (m (i Plicit), m (i expr)) -> (forall j . Permutable j => (i :.: j) expr -> m ((i :.: j) expr)) -> m (i expr)
(pl, a) >-> b = liftA2 (S.>->) <$> (liftA2 (,) <$> pl <*> a) <*> (getC <$> b (C (pure id)))

infixr 6 >->

(-->) :: (Applicative m, S.Type expr, Permutable i) => m (i expr) -> m (i expr) -> m (i expr)
a --> b = (pure (pure Ex), a) >-> const (weakens <$> b)

infixr 6 -->

(==>) :: (Applicative m, S.Type expr, Permutable i) => m (i expr) -> (forall j . Permutable j => (i :.: j) expr -> m ((i :.: j) expr)) -> m (i expr)
a ==> b = (pure (pure Im), a) >-> b

infixr 6 ==>


-- Prob

ex :: (Applicative m, S.Prob expr, Permutable i) => m (i expr) -> (forall j . Permutable j => (i :.: j) expr -> m ((i :.: j) expr)) -> m (i expr)
ex t f = liftA2 S.ex <$> t <*> (getC <$> f (C (pure id)))

(===) :: (Applicative m, S.Prob expr) => m expr ::: m expr -> m expr ::: m expr -> m expr
(tm1 ::: ty1) === (tm2 ::: ty2) = (S.===) <$> ((:::) <$> tm1 <*> ty1) <*> ((:::) <$> tm2 <*> ty2)

infixl 4 ===


-- Elaborator scripts

runScript :: Functor m => Script a m a -> m a
runScript = strengthen . (`getScript` id)

runCScript :: Functor m => (Script (i a) m :.: i) a -> (m :.: i) a
runCScript = mapC runScript

throw
  :: (Applicative m, Applicative hw)
  => (forall h . Permutable h => ((m :.: hw) :.: h) a -> ((m :.: hw) :.: h) w)
  -> m a
  -> (m :.: hw) w
throw k = strengthen . k . liftC . liftC

reset :: Applicative m => Script a m a -> Script w m a
reset m = Script $ \ k -> throw k $ runScript m

resetC :: Applicative m => (Script (i a) m :.: i) a -> (Script w m :.: i) a
resetC = mapC reset

liftScript :: Functor m => m a -> Script w m a
liftScript m = Script $ \ k -> strengthen (k (liftC (liftC m)))

liftCScript :: Functor m => (m :.: i) a -> (Script w m :.: i) a
liftCScript = mapC liftScript

newtype Script t m a = Script
  { getScript
    :: forall hw
    .  Permutable hw
    => (forall h . Permutable h => ((m :.: hw) :.: h) a -> ((m :.: hw) :.: h) t)
    -> (m :.: hw) t
  }

instance Functor m => Functor (Script t m) where
  fmap f (Script run) = Script $ \ k -> run (k . fmap f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Script t m) where
  pure a = Script $ \ k -> strengthen (k (pure a))
  {-# INLINE pure #-}

  Script f <*> Script a = Script $ \ k -> f $ \ f' -> assocL $ a $ \ a' -> assocLR (k (assocR (liftC f') <*> assocRL a'))
  {-# INLINE (<*>) #-}
