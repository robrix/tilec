{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module S.Context
( Ctx(..)
, abstractFin
, instantiateFin
, (!)
) where

import Bound.Scope
import S.Syntax

compose :: Either () (Fin n) -> Fin ('S n)
compose = either (const FZ) FS

decompose :: Fin ('S n) -> Either () (Fin n)
decompose = \case
  FZ   -> Left ()
  FS n -> Right n

abstractFin :: Monad f => f (Fin ('S n)) -> Scope () f (Fin n)
abstractFin = abstractEither decompose

instantiateFin :: Monad f => Scope () f (Fin n) -> f (Fin ('S n))
instantiateFin = instantiateEither (pure . compose)


data Ctx tm ty (n :: N) where
  CNil :: Ctx tm ty 'Z
  (:-) :: Ctx tm ty n -> Maybe (tm (Fin n)) ::: ty (Fin n) -> Ctx tm ty ('S n)

infixl 5 :-

(!) :: (Functor tm, Functor ty) => Ctx tm ty n -> Fin n -> Maybe (tm (Fin n)) ::: ty (Fin n)
(ctx :- t) ! n = case n of
  FZ   -> let tm ::: ty = t       in fmap (fmap FS) tm ::: fmap FS ty
  FS n -> let tm ::: ty = ctx ! n in fmap (fmap FS) tm ::: fmap FS ty
CNil       ! n = case n of {}

infixl 9 !
