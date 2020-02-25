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

newtype PrettyC = PrettyC { runPrettyC :: Last Int -> (Last Int, Endo String) }
  deriving (Semigroup)

instance Show PrettyC where
  showsPrec _ (PrettyC run) = appEndo (snd (run (Last 0)))

instance Var Int PrettyC where
  var = word . (showChar '_' .) . shows

instance Let Int PrettyC where
  let' (tm ::: ty) b = fresh (\ v -> kw "let" <+> var v <+> kw "=" <+> tm <+> kw ":" <+> ty <+> kw "in" <+> b v)

instance Lam Int PrettyC where
  lam f  = fresh $ \ v -> kw "\\" <+> var v <+> kw "." <+> f v
  f $$ a = f <+> a

instance Type Int PrettyC where
  type' = kw "Type"
  pi' t f = fresh $ \ v -> parens (var v <+> kw ":" <+> t) <+> kw "->" <+> f v


word :: ShowS -> PrettyC
word s = PrettyC (, Endo s)

kw :: String -> PrettyC
kw = word . showString

(<+>) :: PrettyC -> PrettyC -> PrettyC
l <+> r = l <> word (showChar ' ') <> r

infixr 6 <+>

parens :: PrettyC -> PrettyC
parens c = kw "(" <> c <> kw ")"

fresh :: (Int -> PrettyC) -> PrettyC
fresh f = PrettyC $ \ v -> runPrettyC (f (getLast v)) ((1 +) <$> v)
