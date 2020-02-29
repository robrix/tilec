{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Term
( tests
) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (for_)
import Data.Set as Set
import Hedgehog as H
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Tile.Term

tests :: TestTree
tests = testGroup "Term"
  [ testProperty "reflexivity of ==" . property $ do
    t <- forAllLabelled (runReaderT term 0)
    t H.=== t
  , testProperty "counterexamples of ==" . property $ do
    t <- forAllLabelled (runReaderT term 0)
    t H./== lam Im (const t)
    lam Im (const t) H./== t
  ]

forAllLabelled :: Show a => WriterT (Set LabelName) Gen a -> PropertyT IO a
forAllLabelled gen = do
  (t, labels) <- forAll (runWriterT gen)
  t <$ for_ labels label

term :: ReaderT Int (WriterT (Set LabelName) Gen) (Term Int Int)
term = go where
  go = ask >>= \ i -> recursive (small . choice)
    ((if i > 0 then ((localVar <* tag "var") :) else id) [ type' <$ tag "type", err <$> string (linear 0 10) alphaNum <* tag "err" ])
    [ subtermM2 go (local succ go) (\ t b -> let' t (const b) <$ tag "let")
    , subtermM (local succ go) (\ b -> lam <$> plicit <*> pure (const b) <* tag "lam")
    , subtermM2 go go (\ f a -> f $$ a <$ tag "$$")
    , subtermM2 go (local succ go) (\ t b -> (>-> const b) . (, t) <$> plicit <* tag ">->")
    , subtermM2 go (local succ go) (\ t b -> (t `ex` const b) <$ tag "ex")
    , subtermM2 go go (\ m1 t1 -> subtermM2 go go (\ m2 t2 -> ((m1 ::: t1) Gen.=== (m2 ::: t2)) <$ tag "==="))
    ]
