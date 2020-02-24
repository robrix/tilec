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

import           Bound.Scope
import qualified S.Core as Core
import           S.Syntax

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


data Ctx (n :: N) where
  CNil :: Ctx 'Z
  (:-) :: Ctx n -> Maybe (Core.Term (Fin n)) ::: Core.Term (Fin n) -> Ctx ('S n)

infixl 5 :-

(!) :: Ctx n -> Fin n -> Maybe (Core.Term (Fin n)) ::: Core.Term (Fin n)
(ctx :- t) ! n = case n of
  FZ   -> let tm ::: ty = t       in fmap (fmap FS) tm ::: fmap FS ty
  FS n -> let tm ::: ty = ctx ! n in fmap (fmap FS) tm ::: fmap FS ty
CNil       ! n = case n of {}

infixl 9 !
