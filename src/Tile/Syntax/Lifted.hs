{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax.Lifted
( Permutable
, S.Syntax
  -- * Var
, S.Var
, var
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
import           Tile.Plicit
import qualified Tile.Syntax as S
import           Tile.Type

type Permutable f = (Applicative f, Distributive f)

-- Var

var :: (Applicative m, Functor i, S.Var v expr) => i v -> (m :.: i) expr
var = C . pure . fmap S.var


-- Let

let'
  :: (Applicative m, S.Let v expr, Permutable i)
  => (m :.: i) expr ::: (m :.: i) expr
  -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr)
  -> (m :.: i) expr
let' v f = let'' v (f . C . pure)

let''
  :: (Applicative m, S.Let v expr, Permutable i)
  => (m :.: i) expr ::: (m :.: i) expr
  -> (forall j . Permutable j => j v -> (m :.: i :.: j) expr)
  -> (m :.: i) expr
let'' (tm ::: ty) f = S.let' <$> ((:::) <$> tm <*> ty) <*> mapC (fmap getC) (f id)


-- Lam

lam :: (Applicative m, S.Lam v expr, Permutable i) => (m :.: i) Plicit -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
lam p f = S.lam <$> p <*> mapC (fmap getC) (f (C (pure id)))

($$) :: (Applicative m, S.Lam v expr) => m expr -> m expr -> m expr
($$) = liftA2 (S.$$)

infixl 9 $$


-- Type

type' :: (Applicative m, S.Type v expr) => m expr
type' = pure S.type'

(>->) :: (Applicative m, S.Type v expr, Permutable i) => ((m :.: i) Plicit, (m :.: i) expr) -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
(pl, a) >-> b = (S.>->) <$> ((,) <$> pl <*> a) <*> mapC (fmap getC) (b (C (pure id)))

infixr 6 >->

(-->) :: (Applicative m, S.Type v expr, Permutable i) => (m :.: i) expr -> (m :.: i) expr -> (m :.: i) expr
a --> b = (pure Ex, a) >-> const (weakens b)

infixr 6 -->

(==>) :: (Applicative m, S.Type v expr, Permutable i) => (m :.: i) expr -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
a ==> b = (pure Im, a) >-> b

infixr 6 ==>


-- Prob

ex :: (Applicative m, S.Prob v expr, Permutable i) => (m :.: i) expr -> (forall j . Permutable j => (i :.: j) v -> (m :.: i :.: j) expr) -> (m :.: i) expr
ex t f = S.ex <$> t <*> mapC (fmap getC) (f (C (pure id)))

(===) :: (Applicative m, S.Prob v expr) => m expr ::: m expr -> m expr ::: m expr -> m expr
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
