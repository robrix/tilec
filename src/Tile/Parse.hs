module Tile.Parse
( parse
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Parser.Church
import Control.Carrier.Throw.Either
import Control.Effect.Parser.Notice
import Data.HashSet (HashSet, fromList)
import Data.Semilattice.Lower
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Tile.Syntax

parse :: Type v t => String -> Either Notice t
parse s = run (runThrow (runParserWithString lowerBound s type_))


identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

type_ :: (Monad m, TokenParsing m, Type v t) => m t
type_ = type' <$ reserve identifierStyle "Type"

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  ]
