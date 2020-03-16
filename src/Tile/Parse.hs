{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Parse
( parse
, parseString
, parseFile
, ParseC(..)
, expr_
) where

import           Control.Algebra
import           Control.Carrier.Parser.Church as Parser
import           Control.Carrier.Reader
import           Control.Effect.Cut
import           Control.Effect.NonDet
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

parse :: forall v m a sig . Has (Throw Notice) sig m => Path -> String -> ParseC v m a -> m a
parse path s = runReader (mempty @(Map.Map String v)) . runParser (const pure) failure failure (Input lowerBound s) . runParseC where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> ParseC v m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> ParseC v m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p

newtype ParseC v m a = ParseC { runParseC :: ParserC (ReaderC (Map.Map String v) m) a }
  deriving (Algebra (Parser :+: Cut :+: NonDet :+: Reader (Map.Map String v) :+: sig), Alternative, Applicative, Functor, Monad)

instance MonadTrans (ParseC v) where
  lift = ParseC . lift . lift

deriving instance Parsing      (ParseC v m)
deriving instance CharParsing  (ParseC v m)
deriving instance TokenParsing (ParseC v m)

class Monad m => Suspending a m | m -> a where
  sleaf :: Input -> a -> m a
  snil  :: Parser.Err -> m a
  sfail :: Parser.Err -> m a
  resume :: (Input -> a -> m b) -> (Parser.Err -> m b) -> (Parser.Err -> m b) -> m a -> m b

instance Suspending a m => Suspending a (ReaderC r m) where
  sleaf i = lift . sleaf i
  snil    = lift . snil
  sfail   = lift . sfail
  resume leaf nil fail m = ReaderC $ \ r -> resume (\ i -> runReader r . leaf i) (runReader r . nil) (runReader r . fail) (runReader r m)

instance (Monad m, Var v a m) => Var v a (ParseC v m) where
  var = lift . var

instance (Suspending a m, Lam v a m) => Lam v a (ParseC v m) where
  lam p f = ParseC $ ParserC $ \ leaf nil fail input ->
    -- we can’t hide the context resulting from the parser produced by f in a, so we’ll hide it in m instead
    resume leaf nil fail $ lam p (runParser sleaf snil sfail input . runParseC . f)

  f $$ a = ParseC $ ParserC $ \ leaf nil fail input ->
    resume leaf nil fail $ runParser sleaf snil sfail input (runParseC f) $$ runParser sleaf snil sfail input (runParseC a)

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
