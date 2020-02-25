{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module S.Syntax.Pretty
( PrettyC(..)
) where

import Data.Monoid (Endo(..))
import Data.Semigroup (Last(..))
import S.Syntax
import S.Syntax.Classes

newtype PrettyC a = PrettyC { runPrettyC :: Last Int -> (Last Int, Endo String) }
  deriving (Semigroup)

instance Show (PrettyC a) where
  showsPrec _ (PrettyC run) = appEndo (snd (run (Last 0)))

instance Show a => Var a PrettyC where
  var = word . (showChar '_' .) . shows

instance (Num a, Show a) => Let a PrettyC where
  let' (tm ::: ty) b = fresh (\ v -> kw "let" <+> var v <+> kw "=" <+> tm <+> kw ":" <+> ty <+> kw "in" <+> b v)

instance (Num a, Show a) => Lam a PrettyC where
  lam f  = fresh $ \ v -> kw "\\" <+> var v <+> kw "." <+> f v
  f $$ a = f <+> a

instance (Num a, Show a) => Type a PrettyC where
  type' = kw "Type"
  pi' t f = fresh $ \ v -> parens (var v <+> kw ":" <+> t) <+> kw "->" <+> f v


word :: ShowS -> PrettyC a
word s = PrettyC (, Endo s)

kw :: String -> PrettyC a
kw = word . showString

(<+>) :: PrettyC a -> PrettyC a -> PrettyC a
l <+> r = l <> word (showChar ' ') <> r

infixr 6 <+>

parens :: PrettyC a -> PrettyC a
parens c = kw "(" <> c <> kw ")"

fresh :: Num a => (a -> PrettyC a) -> PrettyC a
fresh f = PrettyC $ \ v -> runPrettyC (f (fromIntegral (getLast v))) ((1 +) <$> v)
