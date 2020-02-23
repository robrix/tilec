{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module S.Elab
( check
, infer
, Ctx(..)
) where

import           Bound.Var
import qualified S.Core as Core
import qualified S.Problem as Problem
import           S.Syntax

check :: MonadFail m => Problem.Term a -> Core.Term a -> m (Core.Term a ::: Core.Term a)
check _ _ = fail "unimplemented"

infer :: MonadFail m => Problem.Term a -> m (Core.Term a ::: Core.Term a)
infer _ = fail "unimplemented"


data Ctx a where
  CNil :: Ctx a
  (:-) :: Ctx a -> Core.Term a -> Ctx (Var () a)

infixl 5 :-
