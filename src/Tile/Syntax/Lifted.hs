{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Syntax.Lifted
( S.Syntax
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
, throw
, reset
, resetC
, liftScript
, Script(..)
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import           Control.Applicative (liftA2)
import           Data.Functor.Identity
import           Tile.Functor.Compose
import           Tile.Syntax ((:::)(..), Plicit(..), plicit)
import qualified Tile.Syntax as S

var :: (Applicative m, Extends env j) => env expr -> m (j expr)
var = weaken . pure


-- Let

let'
  :: (Applicative m, S.Let expr, Permutable env)
  => m (env expr) ::: m (env expr)
  -> (forall env' . Extends env env' => env' expr -> m (env' expr))
  -> m (env expr)
let' v f = let'' v (f . C . pure)

let''
  :: (Applicative m, S.Let expr, Permutable env)
  => m (env expr) ::: m (env expr)
  -> (forall env' . Permutable env' => env' expr -> m ((env :.: env') expr))
  -> m (env expr)
let'' (tm ::: ty) f = liftA2 S.let' <$> (liftA2 (:::) <$> tm <*> ty) <*> (getC <$> f id)


-- Lam

lam :: (Applicative m, S.Lam expr, Permutable env) => m (env Plicit) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
lam p f = liftA2 S.lam <$> p <*> (getC <$> f (C (pure id)))

($$) :: (Applicative m, S.Lam expr) => m expr -> m expr -> m expr
($$) = liftA2 (S.$$)

infixl 9 $$


-- Type

type' :: (Applicative m, S.Type expr) => m expr
type' = pure S.type'

(>->) :: (Applicative m, S.Type expr, Permutable env) => (m (env Plicit), m (env expr)) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
(pl, a) >-> b = liftA2 (S.>->) <$> (liftA2 (,) <$> pl <*> a) <*> (getC <$> b (C (pure id)))

infixr 6 >->

(-->) :: (Applicative m, S.Type expr, Permutable env) => m (env expr) -> m (env expr) -> m (env expr)
a --> b = (pure (pure Ex), a) >-> const (weaken b)

infixr 6 -->

(==>) :: (Applicative m, S.Type expr, Permutable env) => m (env expr) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
a ==> b = (pure (pure Im), a) >-> b

infixr 6 ==>


-- Prob

ex :: (Applicative m, S.Prob expr, Permutable env) => m (env expr) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
ex t f = liftA2 S.ex <$> t <*> (getC <$> f (C (pure id)))

(===) :: (Applicative m, Permutable env, S.Prob expr) => m (env expr) ::: m (env expr) -> m (env expr) ::: m (env expr) -> m (env expr)
(tm1 ::: ty1) === (tm2 ::: ty2) = liftA2 (S.===) <$> (liftA2 (:::) <$> tm1 <*> ty1) <*> (liftA2 (:::) <$> tm2 <*> ty2)

infixl 4 ===


-- Elaborator scripts

runScript :: Functor m => Script a m a -> m a
runScript = fmap runIdentity . (`getScript` id)

throw
  :: (Applicative m, Permutable env)
  => (forall env' . Extends env env' => m (env' a) -> m (env' w))
  -> m a
  -> m (env w)
throw k = strengthen . k . fmap pure

reset :: Applicative m => Script a m a -> Script w m a
reset m = Script $ \ k -> throw k $ runScript m

resetC :: Applicative m => (Script (env a) m :.: env) a -> (Script w m :.: env) a
resetC = mapC reset

liftScript :: Functor m => m a -> Script w m a
liftScript m = Script $ \ k -> strengthen (k (pure <$> m))

newtype Script t m a = Script
  { getScript
    :: forall env
    .  Permutable env
    => (forall env' . Extends env env' => m (env' a) -> m (env' t))
    -> m (env t)
  }

instance Functor m => Functor (Script t m) where
  fmap f (Script run) = Script $ \ k -> run (k . fmap (fmap f))
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Script t m) where
  pure a = Script $ \ k -> strengthen (k (pure (pure a)))
  {-# INLINE pure #-}

  f <*> a = Script (go f a)
    where
    go :: forall env a b . Permutable env => Script t m (a -> b) -> Script t m a -> (forall env' . Extends env env' => m (env' b) -> m (env' t)) -> m (env t)
    go (Script f) (Script a) k = f $ \ (f' :: m (env' (a -> b))) -> a $ \ (a' :: m (env'' a)) ->
      getTr @env @env' @env'' <$> k (Tr <$> liftA2 (<*>) (weaken f') a')
  {-# INLINE (<*>) #-}
