{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module S.Elab
( check
, infer
, Ctx(..)
) where

import           Bound.Scope
import           Bound.Var
import qualified S.Core as Core
import qualified S.Problem as Problem
import           S.Syntax

check :: (MonadFail m, Eq a) => Ctx a -> Problem.Term a ::: Core.Term a -> m (Core.Term a)
check ctx = \case
  Problem.Abs b ::: Core.Pi ta tb -> do
    b' <- check (ctx :- (Nothing ::: ta)) (fromScope b ::: fromScope tb)
    pure (Core.Abs (toScope b'))

  Problem.Let t v b ::: tb -> do
    t' <- check ctx (t ::: Core.Type)
    v' <- check ctx (v ::: t')
    b' <- check (ctx :- (Just v' ::: t')) (fromScope b ::: fmap pure tb)
    pure (Core.Let t' v' (toScope b'))

  tm ::: ty -> do
    tm' ::: ty' <- infer ctx tm
    if ty' == ty then
      pure tm'
    else
      fail "type mismatch"

infer :: (MonadFail m, Eq a) => Ctx a -> Problem.Term a -> m (Core.Term a ::: Core.Term a)
infer ctx = \case
  Problem.Type -> pure (Core.Type ::: Core.Type)

  Problem.Pi t b -> do
    t' <- check ctx (t ::: Core.Type)
    b' <- check (ctx :- (Nothing ::: t')) (fromScope b ::: Core.Type)
    pure (Core.Pi t' (toScope b') ::: Core.Type)

  _ -> fail "no rule to infer"


data Ctx a where
  CNil :: Ctx a
  (:-) :: Ctx a -> Maybe (Core.Term a) ::: Core.Term a -> Ctx (Var () a)

infixl 5 :-
