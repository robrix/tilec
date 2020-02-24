{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module S.Syntax.Check
( CheckC(..)
) where

import S.Syntax
import S.Syntax.Classes

newtype CheckC a = CheckC { runCheckC :: () }

instance Def CheckC CheckC (Fin 'Z) CheckC where
  def _ = CheckC ()
