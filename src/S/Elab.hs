{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module S.Elab
( check
, infer
, Ctx(..)
) where

import           Bound.Scope
import           S.Context
import qualified S.Core as Core
import qualified S.Problem as Problem
import           S.Syntax

check :: MonadFail m => Ctx n -> Problem.Term (Fin n) ::: Core.Term (Fin n) -> m (Core.Term (Fin n))
check ctx = \case
  Problem.Lam b ::: Core.Pi ta tb -> do
    b' <- check (ctx :- (Nothing ::: ta)) (instantiateFin b ::: instantiateFin tb)
    pure (Core.Lam (abstractFin b'))

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
  Problem.Var n -> let _ ::: ty = ctx ! n in pure (pure n ::: ty)

  Problem.Lam _ -> fail "no rule to infer lambda abstractions"

  f Problem.:$ a -> do
    f' ::: tf <- infer ctx f
    case tf of
      Core.Pi t b -> do
        a' <- check ctx (a ::: t)
        pure (f' Core.$$ a' ::: instantiateEither (either (const a') pure) b)
      _ -> fail "expected function type"

  Problem.Type -> pure (Core.Type ::: Core.Type)

  Problem.Pi t b -> do
    t' <- check ctx (t ::: Core.Type)
    b' <- check (ctx :- (Nothing ::: t')) (instantiateFin b ::: Core.Type)
    pure (Core.Pi t' (abstractFin b') ::: Core.Type)

  _ -> fail "no rule to infer"
