{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Parse
( parse
, parseString
, parseFile
, ParseC(..)
, runEnv
, EnvC(..)
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
import           Data.Distributive
import           Data.Functor
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Functor.Compose
import           Tile.Syntax

parse :: forall i v m a sig . Has (Throw Notice) sig m => Path -> String -> ParseC i v m a -> m a
parse path s = runParser (const pure) failure failure (Input lowerBound s) . runParseC where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> ParseC i v m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> ParseC i v m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p

newtype ParseC i v m a = ParseC { runParseC :: ParserC m a }
  deriving (Algebra (Parser :+: Cut :+: NonDet :+: sig), Alternative, Applicative, CharParsing, Functor, Monad, MonadPlus, MonadTrans, Parsing, TokenParsing)

deriving instance Algebra sig m => MonadFail (ParseC i v m)

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

instance (Monad m, Var v a m) => Var v a (ParseC i v m) where
  var = lift . var

instance (Monad m, Free v a m) => Free v a (ParseC i v m) where
  free = lift . free

suspend :: Suspending a m => Input -> ParserC m a -> m a
suspend = runParser sleaf snil sfail
{-# INLINE suspend #-}

instance (Suspending a m, Lam v a m) => Lam v a (ParseC i v m) where
  lam p f = ParseC $ ParserC $ \ leaf nil fail input ->
    -- we can’t hide the context resulting from the parser produced by f in a, so we’ll hide it in m instead
    resume leaf nil fail $ lam p (suspend input . runParseC . f)

  f $$ a = ParseC $ ParserC $ \ leaf nil fail input ->
    resume leaf nil fail $ suspend input (runParseC f) $$ suspend input (runParseC a)


runEnv :: Map.Map String (i v) -> EnvC i v m a -> m a
runEnv m = runReader m . runEnvC

newtype EnvC i v m a = EnvC { runEnvC :: ReaderC (Map.Map String (i v)) m a }
  deriving (Algebra (Reader (Map.Map String (i v)) :+: sig), Alternative, Applicative, Functor, Monad, MonadFail, MonadPlus, MonadTrans)

instance Distributive m => Distributive (EnvC i v m) where
  distribute m = EnvC . ReaderC $ \ r -> distribute (runEnv r <$> m)
  {-# INLINE distribute #-}

  collect f m = EnvC . ReaderC $ \ r -> collect (runEnv r . f) m
  {-# INLINE collect #-}

liftEnvC0 :: m a -> EnvC i v m a
liftEnvC0 = EnvC . ReaderC . const

liftEnvC1 :: (m a -> m' a') -> EnvC i v m a -> EnvC i v m' a'
liftEnvC1 f m = EnvC . ReaderC $ \ r -> f (runReader r (runEnvC m))

instance Parsing m => Parsing (EnvC i v m) where
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

instance CharParsing m => CharParsing (EnvC i v m) where
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

instance TokenParsing m => TokenParsing (EnvC i v m) where
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


expr_ :: (Permutable i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Lam v a expr, Type v a expr) => (m :.: i) (expr a)
expr_ = type_ <|> var_ <|> lam_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Functor i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Var v a expr) => (m :.: i) (expr a)
var_ = C $ do
  v <- identifier_
  v' <- asks (Map.lookup v)
  maybe (unexpected "free variable") varA v'

lam_ :: (Permutable i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Lam v a expr, Type v a expr) => (m :.: i) (expr a)
lam_ = C $ token (char '\\') *> do
  i <- identifier_
  void (token (char '.'))
  getC (lamA Ex (\ v -> C (asks (Map.insert i v . fmap liftC) >>= \ env -> runEnv env (getC expr_))))

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
