{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Tile.Functor.Compose
import           Tile.Syntax

parse :: forall v m a sig . Has (Throw Notice) sig m => Path -> String -> ParseC v m a -> m a
parse path s = runReader mempty . runParser (const pure) failure failure (Input lowerBound s) . runParseC where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> ParseC v m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> ParseC v m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p

newtype ParseC v m a = ParseC { runParseC :: ParserC (ReaderC (Map.Map String v) m) a }
  deriving (Algebra (Parser :+: Cut :+: NonDet :+: Reader (Map.Map String v) :+: sig), Alternative, Applicative, CharParsing, Functor, Monad, Parsing, TokenParsing)

instance MonadTrans (ParseC v) where
  lift = ParseC . lift . lift

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

instance (Monad m, Free v a m) => Free v a (ParseC v m) where
  free = lift . free

suspend :: Suspending a m => Input -> ParserC m a -> m a
suspend = runParser sleaf snil sfail
{-# INLINE suspend #-}

instance (Suspending a m, Lam v a m) => Lam v a (ParseC v m) where
  lam p f = ParseC $ ParserC $ \ leaf nil fail input ->
    -- we can’t hide the context resulting from the parser produced by f in a, so we’ll hide it in m instead
    resume leaf nil fail $ lam p (suspend input . runParseC . f)

  f $$ a = ParseC $ ParserC $ \ leaf nil fail input ->
    resume leaf nil fail $ suspend input (runParseC f) $$ suspend input (runParseC a)

expr_ :: (Applicative i, Has (Reader (Map.Map String v)) sig m, TokenParsing m, Type v a expr, MonadFail m) => (m :.: i) (expr a)
expr_ = type_ <|> var_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Applicative i, Has (Reader (Map.Map String v)) sig m, TokenParsing m, Var v a expr, MonadFail m) => (m :.: i) (expr a)
var_ = C $ do
  v <- identifier_
  v' <- asks (Map.lookup v)
  maybe (fail "free variable") (varA . pure) v'

type_ :: (Monad m, Applicative i, TokenParsing m, Type v a expr) => (m :.: i) (expr a)
type_ = C $ pure type' <$ reserve identifierStyle "Type"

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  ]
