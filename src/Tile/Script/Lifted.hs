{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Script.Lifted
( -- * Elaborator scripts
  runScript
, evalScript
, throw
, reset
, liftScript
, Script(..)
, meta
, intro
, letbind
) where

import           Control.Applicative (liftA2)
import           Data.Functor.Identity
import           Tile.Functor.Compose
import qualified Tile.Syntax as S
import           Tile.Syntax.Lifted

runScript :: Permutable env => (forall env' . Extends env env' => m (env' a) -> m (env' t)) -> Script t m a -> m (env t)
runScript k s = getScript s k

evalScript :: Functor m => Script t m t -> m t
evalScript = fmap runIdentity . runScript id

throw
  :: (Applicative m, Permutable env)
  => (forall env' . Extends env env' => m (env' a) -> m (env' a'))
  -> m a
  -> m (env a')
throw k = strengthen . k . fmap pure

reset :: Applicative m => Script t m t -> Script t' m t
reset m = Script $ \ k -> throw k $ evalScript m

liftScript :: Functor m => m t -> Script t' m t
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

meta :: (Applicative m, S.Prob t) => m t -> Script t m t
meta ty = Script $ \ k -> ex (pure <$> ty) (k . pure)

intro :: (Applicative m, S.Lam t) => m Plicit -> Script t m t
intro p = Script $ \ k -> lam (pure <$> p) (k . pure)

letbind :: (Applicative m, S.Let t) => m t ::: m t -> Script t m t
letbind (tm ::: ty) = Script $ \ k -> let' (fmap pure tm ::: fmap pure ty) (k . pure)
