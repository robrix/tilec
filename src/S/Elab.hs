{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module S.Elab
( check
, infer
, Ctx(..)
) where

import           Bound.Scope
import qualified S.Core as Core
import qualified S.Problem as Problem
import           S.Syntax

check :: MonadFail m => Ctx n -> Problem.Term (Fin n) ::: Core.Term (Fin n) -> m (Core.Term (Fin n))
check ctx = \case
  Problem.Abs b ::: Core.Pi ta tb -> do
    b' <- check (ctx :- (Nothing ::: ta)) (instantiateFin b ::: instantiateFin tb)
    pure (Core.Abs (abstractFin b'))

  Problem.Let t v b ::: tb -> do
    t' <- check ctx (t ::: Core.Type)
    v' <- check ctx (v ::: t')
    b' <- check (ctx :- (Just v' ::: t')) (instantiateFin b ::: fmap FS tb)
    pure (Core.Let t' v' (abstractFin b'))

  tm ::: ty -> do
    tm' ::: ty' <- infer ctx tm
    if ty' == ty then
      pure tm'
    else
      fail "type mismatch"

infer :: MonadFail m => Ctx n -> Problem.Term (Fin n) -> m (Core.Term (Fin n) ::: Core.Term (Fin n))
infer ctx = \case
  Problem.Type -> pure (Core.Type ::: Core.Type)

  Problem.Pi t b -> do
    t' <- check ctx (t ::: Core.Type)
    b' <- check (ctx :- (Nothing ::: t')) (instantiateFin b ::: Core.Type)
    pure (Core.Pi t' (abstractFin b') ::: Core.Type)

  _ -> fail "no rule to infer"


data N = Z | S N


data Fin (n :: N) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)

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
