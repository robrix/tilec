{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
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
, ilam
, ($$)
, ($$?)
  -- * Type
, S.Type
, type'
, (->>)
, (=>>)
, (-->)
  -- * Prob
, S.Prob
, ex
, (===)
  -- * Modules
, S.Def
  -- * Re-exports
, (:::)(..)
) where

import           Control.Applicative (liftA2)
import           Tile.Functor.Compose
import           Tile.Syntax ((:::)(..))
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

lam :: (Applicative m, S.Lam expr, Permutable env) => (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
lam f = fmap S.lam . getC <$> f (C (pure id))

ilam :: (Applicative m, S.Lam expr, Permutable env) => (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
ilam f = fmap S.ilam . getC <$> f (C (pure id))

($$) :: (Applicative m, Applicative env, S.Lam expr) => m (env expr) -> m (env expr) -> m (env expr)
f $$ a = liftA2 (liftA2 (S.$$)) f a

($$?) :: (Applicative m, Applicative env, S.Lam expr) => m (env expr) -> m (env expr) -> m (env expr)
f $$? a = liftA2 (liftA2 (S.$$?)) f a

infixl 9 $$, $$?


-- Type

type' :: (Applicative m, Applicative env, S.Type expr) => m (env expr)
type' = pure (pure S.type')

(->>) :: (Applicative m, S.Type expr, Permutable env) => m (env expr) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
a ->> b = liftA2 (S.->>) <$> a <*> (getC <$> b (C (pure id)))

infixr 6 ->>

(=>>) :: (Applicative m, S.Type expr, Permutable env) => m (env expr) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
a =>> b = liftA2 (S.=>>) <$> a <*> (getC <$> b (C (pure id)))

infixr 6 =>>

(-->) :: (Applicative m, S.Type expr, Permutable env) => m (env expr) -> m (env expr) -> m (env expr)
a --> b = a ->> const (weaken b)

infixr 6 -->


-- Prob

ex :: (Applicative m, S.Prob expr, Permutable env) => m (env expr) -> (forall env' . Extends env env' => env' expr -> m (env' expr)) -> m (env expr)
ex t f = liftA2 S.ex <$> t <*> (getC <$> f (C (pure id)))

(===) :: (Applicative m, Permutable env, S.Prob expr) => m (env expr) ::: m (env expr) -> m (env expr) ::: m (env expr) -> m (env expr)
(tm1 ::: ty1) === (tm2 ::: ty2) = liftA2 (S.===) <$> (liftA2 (:::) <$> tm1 <*> ty1) <*> (liftA2 (:::) <$> tm2 <*> ty2)

infixl 4 ===
