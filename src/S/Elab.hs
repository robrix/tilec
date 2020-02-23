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

check :: MonadFail m => Ctx a -> Problem.Term a ::: Core.Term a -> m (Core.Term a ::: Core.Term a)
check ctx = \case
  Problem.Abs b ::: Core.Pi ta tb -> do
    b' ::: tb' <- check (ctx :- ta) (fromScope b ::: fromScope tb)
    pure (Core.Abs (toScope b') ::: Core.Pi ta (toScope tb'))

  _ -> fail "unimplemented"

infer :: MonadFail m => Problem.Term a -> m (Core.Term a ::: Core.Term a)
infer _ = fail "unimplemented"


data Ctx a where
  CNil :: Ctx a
  (:-) :: Ctx a -> Core.Term a -> Ctx (Var () a)

infixl 5 :-
