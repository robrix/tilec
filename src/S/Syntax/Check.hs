module S.Syntax.Check
( CheckC(..)
) where

newtype CheckC a = CheckC { runCheckC :: () }
