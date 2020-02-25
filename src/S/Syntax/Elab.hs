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

newtype CheckC t m a = CheckC { runCheckC :: Term a a -> [t a] -> m (t a) }
newtype InferC t m a = InferC { runInferC ::             [t a] -> m (t a ::: t a) }

instance (Var Int t, Applicative m) => Var Int (InferC t m) where
  var n = InferC $ \ ctx -> pure (var n ::: ctx !! n)
