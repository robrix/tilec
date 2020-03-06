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
import Data.List.NonEmpty
import Data.Semilattice.Lower
import Text.Parser.Char
import Text.Parser.Token
import Tile.Syntax

parse :: SExpr t => String -> Either Notice t
parse s = run (runThrow (runParserWithString lowerBound s sexpr_))


class SExpr t where
  atom :: NonEmpty Char -> t
  list :: [t] -> t

sexpr_ :: (TokenParsing m, SExpr t) => m t
sexpr_ = list_ <|> atom_

atom_ :: (TokenParsing m, SExpr t) => m t
atom_ = atom <$> token (some1 alphaNum)

list_ :: (TokenParsing m, SExpr t) => m t
list_ = list <$> parens (many sexpr_)


newtype Surface t = Surface { runSurface :: Either String ([t] -> Either String t) }

instance Type v t => SExpr (Surface t) where
  atom s = case toList s of
    "Type" -> Surface . Right $ \case
      [] -> Right type'
      _  -> Left "unexpected arguments to type'"
    "->"   -> Surface . Right $ \case
      []  -> Left "0 arguments given to ->"
      [_] -> Left "1 argument given to ->"
      ts  -> Right $ foldr1 (-->) ts
    other  -> Surface . Left $ "unknown atom: " <> show other

  list _ = Surface (Left "unimplemented: list")
