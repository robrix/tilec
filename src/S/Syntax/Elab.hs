{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module S.Syntax.Elab
( CheckC(..)
, InferC(..)
) where

import S.Syntax
import S.Syntax.Free
import S.Syntax.Classes

newtype CheckC t m = CheckC { runCheckC :: Term Int Int -> [t] -> m t }
newtype InferC t m = InferC { runInferC ::                 [t] -> m (t ::: t) }

instance (Var Int t, Applicative m) => Var Int (InferC t m) where
  var n = InferC $ \ ctx -> pure (var n ::: ctx !! n)
