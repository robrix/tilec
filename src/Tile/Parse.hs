{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Tile.Parse
( parse
, expr_
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Parser.Church
import           Control.Carrier.Reader
import           Control.Carrier.Throw.Either
import           Control.Effect.Parser.Notice
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Text.Parser.Char
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Syntax

parse :: forall v t . (Free v t, Type v t) => String -> Either Notice t
parse s = run (runThrow (runReader (mempty @(Map.Map String v)) (runParserWithString lowerBound s expr_)))


expr_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v t, Type v t) => m t
expr_ = type_ <|> var_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v t) => m t
var_ = do
  v <- identifier_
  asks (maybe (free v) var . Map.lookup v)

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
