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
import           Data.Functor
import           Data.Functor.Identity
import           Data.HashSet (HashSet, fromList)
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Tile.Functor.Compose
import           Tile.Syntax

parse :: Has (Throw Notice) sig m => Path -> String -> ParserC m a -> m a
parse path s = runParser (const pure) failure failure (Input lowerBound s) where
  failure = throwError . errToNotice path lines
  lines = linesFromString s

parseString :: Has (Throw Notice) sig m => String -> ParserC m a -> m a
parseString = parse (Path "(interactive)")

parseFile :: (Has (Throw Notice) sig m, MonadIO m) => Path -> ParserC m a -> m a
parseFile path p = do
  s <- liftIO (readFile (getPath path))
  parse path s p


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


expr :: forall v expr m sig . (Algebra sig m, TokenParsing m, Lam v expr, Type v expr) => m expr
expr = runEnv @Identity @v mempty (strengthen expr_)

expr_ :: (Permutable i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Lam v expr, Type v expr) => (m :.: i) expr
expr_ = type_ <|> var_ <|> lam_

identifier_ :: (Monad m, TokenParsing m) => m String
identifier_ = ident identifierStyle

var_ :: (Functor i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Var v expr) => (m :.: i) expr
var_ = C $ do
  v <- identifier_
  v' <- asks (Map.lookup v)
  maybe (unexpected "free variable") varA v'

lam_ :: (Permutable i, Has (Reader (Map.Map String (i v))) sig m, TokenParsing m, Lam v expr, Type v expr) => (m :.: i) expr
lam_ = C $ token (char '\\') *> do
  i <- identifier_
  void (token (char '.'))
  getC (lamA Ex (\ v -> C (asks (Map.insert i v . fmap liftC) >>= \ env -> runEnv env (getC expr_))))

type_ :: (Monad m, Applicative i, TokenParsing m, Type v expr) => (m :.: i) expr
type_ = C $ pure type' <$ reserve identifierStyle "Type"

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  ]
