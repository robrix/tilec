module Tile.Parse
( SExpr(..)
, sexpr_
) where

import Control.Applicative (Alternative(..))
import Text.Parser.Char
import Text.Parser.Token

class SExpr t where
  atom :: String -> t
  list :: [t] -> t

sexpr_ :: (TokenParsing m, SExpr t) => m t
sexpr_ = list_ <|> atom_

atom_ :: (TokenParsing m, SExpr t) => m t
atom_ = atom <$> token (many alphaNum)

list_ :: (TokenParsing m, SExpr t) => m t
list_ = list <$> parens (many sexpr_)
