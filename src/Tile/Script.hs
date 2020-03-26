{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Tile.Script
( -- * Elaborator scripts
  runScript
, evalScript
, shift
, reset
, Script(..)
, (.:)
, meta
, intro
, letbind
) where

import Control.Monad (ap)
import Tile.Syntax

runScript :: (a -> t) -> Script t a -> t
runScript = flip getScript

evalScript :: Script t t -> t
evalScript = runScript id

shift :: ((a -> t) -> Script t t) -> Script t a
shift f = Script (evalScript . f)

reset :: Script t t -> Script t' t
reset = pure . evalScript

newtype Script t a = Script { getScript :: (a -> t) -> t }
  deriving (Functor)

instance Applicative (Script t) where
  pure = Script . flip ($)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Script t) where
  m >>= f = Script (\ k -> runScript (runScript k . f) m)
  {-# INLINE (>>=) #-}

(.:) :: Def t def => String -> t := t -> Script (def t) t
name .: (ty := tm) = Script $ def (name ::: ty := tm)

infix 3 .:

meta :: Prob t => t -> Script t t
meta = Script . ex

intro :: Lam t => Plicit -> Script t t
intro = Script . lam

letbind :: Let t => t ::: t -> Script t t
letbind = Script . let'
