{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Syntax
( Var(..)
, Let(..)
, Lam(..)
, Type(..)
, (-->)
, Prob(..)
, Err(..)
, Def(..)
) where

import Tile.Type

class Var a expr | expr -> a where
  var :: a -> expr

instance (Var a expr1, Var a expr2) => Var a (expr1, expr2) where
  var a = (var a, var a)

deriving instance (Var a expr1, Var a expr2) => Var a (expr1 ::: expr2)


class Var a expr => Let a expr where
  let' :: expr ::: expr -> (a -> expr) -> expr

instance (Let a expr1, Let a expr2) => Let a (expr1, expr2) where
  let' ((tm1, tm2) ::: (ty1, ty2)) b = (let' (tm1 ::: ty1) (fst . b), let' (tm2 ::: ty2) (snd . b))

deriving instance (Let a expr1, Let a expr2) => Let a (expr1 ::: expr2)


class Var a expr => Lam a expr where
  lam :: (a -> expr) -> expr
  ($$) :: expr -> expr -> expr

  infixl 9 $$

instance (Lam a expr1, Lam a expr2) => Lam a (expr1, expr2) where
  lam b = (lam (fst . b), lam (snd . b))
  (f1, f2) $$ (a1, a2) = (f1 $$ a1, f2 $$ a2)

deriving instance (Lam a expr1, Lam a expr2) => Lam a (expr1 ::: expr2)


class Var a expr => Type a expr where
  type' :: expr
  pi' :: expr -> (a -> expr) -> expr

  infixr 0 `pi'`

instance (Type a expr1, Type a expr2) => Type a (expr1, expr2) where
  type' = (type', type')
  pi' (t1, t2) b = (pi' t1 (fst . b), pi' t2 (snd . b))

deriving instance (Type a expr1, Type a expr2) => Type a (expr1 ::: expr2)

(-->) :: Type a expr => expr -> expr -> expr
a --> b = a `pi'` const b

infixr 0 -->


class Var a expr => Prob a expr where
  ex :: expr -> (a -> expr) -> expr
  (===) :: expr -> expr -> expr

  infixl 4 ===

instance (Prob a expr1, Prob a expr2) => Prob a (expr1, expr2) where
  ex (t1, t2) b = (ex t1 (fst . b), ex t2 (snd . b))
  (tm1, ty1) === (tm2, ty2) = (tm1 === tm2, ty1 === ty2)

deriving instance (Prob a expr1, Prob a expr2) => Prob a (expr1 ::: expr2)


class Err expr where
  err :: String -> expr

instance (Err expr1, Err expr2) => Err (expr1, expr2) where
  err s = (err s, err s)

deriving instance (Err expr1, Err expr2) => Err (expr1 ::: expr2)


class Def tm ty a def | def -> tm ty where
  def :: tm a ::: ty a -> def a


-- FIXME: modules
-- FIXME: packages
