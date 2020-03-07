{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Parse
( parse
, parseString
, parseFile
, Parse(..)
, expr_
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Parser.Church
import           Control.Carrier.Reader
import           Control.Effect.Parser.Lines
import           Control.Effect.Parser.Notice
import           Control.Effect.Parser.Path
import           Control.Effect.Throw
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Syntax

parse :: forall v m a sig . Has (Throw Notice) sig m => Path -> String -> Parse v m a -> m a
parse path s = runReader (mempty @(Map.Map String v)) . runParser (const pure) failure failure (Input lowerBound s) . runParse where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> Parse v m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> Parse v m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p

newtype Parse v m a = Parse { runParse :: ParserC (ReaderC (Map.Map String v) m) a }
  deriving (Alternative, Applicative, Functor, Monad)

deriving instance Algebra sig m => Parsing (Parse v m)
deriving instance Algebra sig m => CharParsing (Parse v m)
deriving instance Algebra sig m => TokenParsing (Parse v m)

instance (Monad m, Var v a m) => Var v a (Parse v m) where
  var = Parse . lift . var

expr_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v a m, Type v a m) => m a
expr_ = type_ <|> var_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Has (Reader (Map.Map String v)) sig m, TokenParsing m, Free v a m) => m a
var_ = do
  v <- identifier_
  v' <- asks (Map.lookup v)
  maybe (free v) var v'

type_ :: (Monad m, TokenParsing m, Type v a m) => m a
type_ = type' <* reserve identifierStyle "Type"

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  ]
