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
import Data.HashSet as HS
import Data.List.NonEmpty as NE
import Data.Semilattice.Lower
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Tile.Syntax

parse :: SExpr t => String -> Either Notice t
parse s = run (runThrow (runParserWithString lowerBound s sexpr_))


class SExpr t where
  atom :: NonEmpty Char -> t
  list :: [t] -> t

sexpr_ :: (Monad m, TokenParsing m, SExpr t) => m t
sexpr_ = list_ <|> atom_

atom_ :: (Monad m, TokenParsing m, SExpr t) => m t
atom_ = atom . NE.fromList <$> ident (IdentifierStyle "identifier" letter (alphaNum <|> char '\'') reservedWords Identifier ReservedIdentifier)

list_ :: (Monad m, TokenParsing m, SExpr t) => m t
list_ = list <$> parens (many sexpr_)


reservedWords :: HashSet String
reservedWords = HS.fromList
  [ "Type"
  , "module"
  , "import"
  ]


newtype Surface t = Surface { runSurface :: Either String ([t] -> Either String t) }

instance Type v t => SExpr (Surface t) where
  atom s = case NE.toList s of
    "Type" -> Surface . Right $ \case
      [] -> Right type'
      _  -> Left "unexpected arguments to type'"
    "->"   -> Surface . Right $ \case
      []  -> Left "0 arguments given to ->"
      [_] -> Left "1 argument given to ->"
      ts  -> Right $ foldr1 (-->) ts
    other  -> Surface . Left $ "unknown atom: " <> show other

  list _ = Surface (Left "unimplemented: list")
