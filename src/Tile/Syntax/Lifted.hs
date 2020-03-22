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
  -- * Re-exports
, (:::)(..)
, Plicit(..)
, plicit
) where

import           Control.Applicative (liftA2)
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

($$) :: (Applicative m, Applicative env, S.Lam expr) => m (env expr) -> m (env expr) -> m (env expr)
($$) = liftA2 (liftA2 (S.$$))

infixl 9 $$


-- Type

type' :: (Applicative m, Applicative env, S.Type expr) => m (env expr)
type' = pure (pure S.type')

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
