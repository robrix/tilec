module Tile.Parse
( parse
, parseString
, parseFile
, expr
) where

import           Control.Algebra
import           Control.Carrier.Parser.Church as Parser
import           Control.Effect.NonDet
import           Control.Effect.Parser.Lines
import           Control.Effect.Parser.Notice
import           Control.Effect.Parser.Path
import           Control.Effect.Throw
import           Control.Monad.IO.Class
import           Data.Foldable (asum)
import           Data.Functor
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Source.Span
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Functor.Compose
import           Tile.Syntax.Lifted

parse :: Has (Throw Notice) sig m => Path -> String -> ParserC m a -> m a
parse path s = runParser (const pure) failure failure (Input (Pos 0 0) s) where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> ParserC m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> ParserC m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p


type Env env expr = Map.Map String (env expr)

expr :: (Algebra sig m, TokenParsing m, Let expr, Lam expr, Type expr) => m expr
expr = run <$> expr_ Map.empty

expr_ :: (Monad m, Permutable env, TokenParsing m, Let expr, Lam expr, Type expr) => Env env expr -> m (env expr)
expr_ env = type_ <|> var_ env <|> lam_ env <|> let_ env

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: TokenParsing m => Env env expr -> m (env expr)
var_ env = asum (map (\ (k, v) -> v <$ token (string k) <?> '‘':k++"’") (Map.toList env))

let_ :: (Monad m, Permutable env, TokenParsing m, Lam expr, Let expr, Type expr) => Env env expr -> m (env expr)
let_ env = keyword "let" *> do
  i <- identifier_ <* keyword "="
  tm <- expr_ env <* keyword ":"
  ty <- expr_ env <* keyword "in"
  let' (pure tm ::: pure ty) (\ v -> expr_ (Map.insert i v (weaken env)))

-- FIXME: lambdas bindng implicit variables

lam_ :: (Monad m, Permutable env, TokenParsing m, Lam expr, Let expr, Type expr) => Env env expr -> m (env expr)
lam_ env = keyword "\\" *> do
  i <- identifier_ <* keyword "."
  lam (\ v -> expr_ (Map.insert i v (weaken env)))

-- FIXME: application

type_ :: (Monad m, Applicative env, TokenParsing m, Type expr) => m (env expr)
type_ = keyword "Type" *> type'

-- FIXME: pi types

keyword :: (Monad m, TokenParsing m) => String -> m ()
keyword s = void (reserve identifierStyle s) <?> s

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  , "let"
  , "in"
  ]
