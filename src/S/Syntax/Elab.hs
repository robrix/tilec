{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax.Elab
( CheckC(..)
, InferC(..)
) where

import S.Context
import S.Syntax
import S.Syntax.Classes

data CheckC tm ty m a where
  CheckC :: { runCheckC :: Ctx tm ty n -> m (tm (Fin n)) } -> CheckC tm ty m (Fin n)


data InferC tm ty m a where
  InferC :: { runInferC :: Ctx tm ty n -> m (tm (Fin n) ::: ty (Fin n)) } -> InferC tm ty m (Fin n)

instance (Var tm (Fin n), Applicative m, Functor tm, Functor ty) => Var (InferC tm ty m) (Fin n) where
  var n = InferC $ \ ctx -> let _ ::: ty = ctx ! n in pure (var n ::: ty)