{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Tile.Parse
( parse
, Parse(..)
, expr_
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Parser.Church
import           Control.Carrier.Reader
import           Control.Effect.Parser.Notice
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Syntax

parse :: forall v t . String -> Parse v t -> Either Notice t
parse s = runReader (mempty @(Map.Map String v)) . runParserWithString lowerBound s . runParse

newtype Parse v t = Parse { runParse :: ParserC (ReaderC Path (ReaderC Lines (ReaderC (Map.Map String v) (Either Notice)))) t }
  deriving (Alternative, Applicative, CharParsing, Functor, Monad, Parsing, TokenParsing)

instance Var v t => Var v (Parse v t) where
  var = Parse . pure . var


expr_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v (m t), Type v t) => m t
expr_ = type_ <|> var_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v (m t)) => m t
var_ = do
  v <- identifier_
  v' <- asks (Map.lookup v)
  maybe (free v) var v'

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
