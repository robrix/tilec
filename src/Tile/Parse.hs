{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Parse
( parse
, parseString
, parseFile
, runEnv
, EnvC(..)
, expr
) where

import           Control.Algebra
import           Control.Carrier.Parser.Church as Parser
import           Control.Carrier.Reader
import           Control.Effect.NonDet
import           Control.Effect.Parser.Lines
import           Control.Effect.Parser.Notice
import           Control.Effect.Parser.Path
import           Control.Effect.Throw
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Distributive
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


runEnv :: Map.Map String expr -> EnvC expr m a -> m a
runEnv m = runReader m . runEnvC

newtype EnvC expr m a = EnvC { runEnvC :: ReaderC (Map.Map String expr) m a }
  deriving (Algebra (Reader (Map.Map String expr) :+: sig), Alternative, Applicative, Functor, Monad, MonadFail, MonadPlus, MonadTrans)

instance Distributive m => Distributive (EnvC expr m) where
  distribute m = EnvC . ReaderC $ \ r -> distribute (runEnv r <$> m)
  {-# INLINE distribute #-}

  collect f m = EnvC . ReaderC $ \ r -> collect (runEnv r . f) m
  {-# INLINE collect #-}

liftEnvC0 :: m a -> EnvC expr m a
liftEnvC0 = EnvC . ReaderC . const

liftEnvC1 :: (m a -> m' a') -> EnvC expr m a -> EnvC expr m' a'
liftEnvC1 f m = EnvC . ReaderC $ \ r -> f (runReader r (runEnvC m))

instance Parsing m => Parsing (EnvC expr m) where
  try = liftEnvC1 try
  {-# INLINE try #-}

  m <?> s = liftEnvC1 (<?> s) m
  {-# INLINE (<?>) #-}

  skipMany = liftEnvC1 skipMany
  {-# INLINE skipMany #-}

  skipSome = liftEnvC1 skipSome
  {-# INLINE skipSome #-}

  unexpected = liftEnvC0 . unexpected
  {-# INLINE unexpected #-}

  eof = liftEnvC0 eof
  {-# INLINE eof #-}

  notFollowedBy = liftEnvC1 notFollowedBy
  {-# INLINE notFollowedBy #-}

instance CharParsing m => CharParsing (EnvC expr m) where
  satisfy = liftEnvC0 . satisfy
  {-# INLINE satisfy #-}

  char = liftEnvC0 . char
  {-# INLINE char #-}

  notChar = liftEnvC0 . notChar
  {-# INLINE notChar #-}

  anyChar = liftEnvC0 anyChar
  {-# INLINE anyChar #-}

  string = liftEnvC0 . string
  {-# INLINE string #-}

  text = liftEnvC0 . text
  {-# INLINE text #-}

instance TokenParsing m => TokenParsing (EnvC expr m) where
  someSpace = liftEnvC0 someSpace
  {-# INLINE someSpace #-}

  nesting = liftEnvC1 nesting
  {-# INLINE nesting #-}

  semi = liftEnvC0 semi
  {-# INLINE semi #-}

  highlight = liftEnvC1 . highlight
  {-# INLINE highlight #-}

  token = liftEnvC1 token
  {-# INLINE token #-}


type Env env expr = Map.Map String (env expr)

expr :: (Algebra sig m, TokenParsing m, Let expr, Lam expr, Type expr) => m expr
expr = run <$> expr_ Map.empty

expr_ :: (Monad m, Permutable env, TokenParsing m, Let expr, Lam expr, Type expr) => Env env expr -> m (env expr)
expr_ env = type_ <|> var_ env <|> lam_ env <|> let_ env

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: TokenParsing m => Env env expr -> m (env expr)
var_ env = asum (map (\ (k, v) -> v <$ token (string k) <?> '‘':k++"’") (Map.toList env))

let_ :: forall env expr m . (Monad m, Permutable env, TokenParsing m, Lam expr, Let expr, Type expr) => Env env expr -> m (env expr)
let_ env = keyword "let" *> do
  i <- identifier_ <* keyword "="
  tm <- expr_ env <* keyword ":"
  ty <- expr_ env <* keyword "in"
  let' (pure tm ::: pure ty) (\ v -> expr_ (Map.insert i v (weaken @_ @env env)))

-- FIXME: lambdas bindng implicit variables

lam_ :: forall env expr m . (Monad m, Permutable env, TokenParsing m, Lam expr, Let expr, Type expr) => Env env expr -> m (env expr)
lam_ env = keyword "\\" *> do
  i <- identifier_ <* keyword "."
  lam (pure (pure Ex)) (\ v -> expr_ (Map.insert i v (weaken @_ @env env)))

-- FIXME: application

type_ :: (Monad m, Applicative env, TokenParsing m, Type expr) => m (env expr)
type_ = type' <$ keyword "Type"

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
