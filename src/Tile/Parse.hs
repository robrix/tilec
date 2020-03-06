module Tile.Parse
( parse
, SExpr(..)
, sexpr_
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Parser.Church
import Control.Carrier.Throw.Either
import Control.Effect.Parser.Notice
import Data.Semilattice.Lower
import Text.Parser.Char
import Text.Parser.Token

parse :: SExpr t => String -> Either Notice t
parse s = run (runThrow (runParserWithString lowerBound s sexpr_))


class SExpr t where
  atom :: String -> t
  list :: [t] -> t

sexpr_ :: (TokenParsing m, SExpr t) => m t
sexpr_ = list_ <|> atom_

atom_ :: (TokenParsing m, SExpr t) => m t
atom_ = atom <$> token (some alphaNum)

list_ :: (TokenParsing m, SExpr t) => m t
list_ = list <$> parens (many sexpr_)
