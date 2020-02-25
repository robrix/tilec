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

newtype CheckC t = CheckC { runCheckC :: Term Int Int -> [t] -> t }
newtype InferC t = InferC { runInferC ::                 [t] -> t ::: t }

instance Var Int t => Var Int (InferC t) where
  var n = InferC $ \ ctx -> var n ::: ctx !! n
