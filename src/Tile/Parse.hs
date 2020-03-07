{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Tile.Parse
( parse
, SExpr(..)
, sexpr_
, Surface(..)
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
import Tile.Syntax as Syn

parse :: SExpr t => String -> Either Notice t
parse s = run (runThrow (runParserWithString lowerBound s sexpr_))


class SExpr t where
  identifier :: String -> t

  type' :: t

  list :: [t] -> t

sexpr_ :: (Monad m, TokenParsing m, SExpr t) => m t
sexpr_ = list_ <|> atom_

atom_ :: (Monad m, TokenParsing m, SExpr t) => m t
atom_ = identifier_ <|> type_

identifier_ :: (Monad m, TokenParsing m, SExpr t) => m t
identifier_ = identifier <$> ident identifierStyle

type_ :: (Monad m, TokenParsing m, SExpr t) => m t
type_ = Tile.Parse.type' <$ reserve identifierStyle "Type"

list_ :: (Monad m, TokenParsing m, SExpr t) => m t
list_ = list <$> parens (many sexpr_)

identifierStyle :: CharParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier


reservedWords :: HashSet String
reservedWords = fromList
  [ "Type"
  , "module"
  , "import"
  ]


newtype Surface t = Surface { runSurface :: [t] -> Either String t }

instance Type v t => SExpr (Surface t) where
  identifier = \case
    "Type" -> Surface $ \case
      [] -> Right Syn.type'
      _  -> Left "unexpected arguments to Type"
    "->"   -> Surface $ \case
      []  -> Left "0 arguments given to ->"
      [_] -> Left "1 argument given to ->"
      ts  -> Right $ foldr1 (-->) ts
    other  -> Surface $ \ _ -> Left $ "unknown atom: " <> show other

  type' = Surface $ \case
    [] -> Right Syn.type'
    _  -> Left "unexpected arguments to Type"

  list _ = Surface (const (Left "unimplemented: list"))
