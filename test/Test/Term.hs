module Test.Term
( tests
) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (for_)
import Data.Set as Set
import Hedgehog as H
import Hedgehog.Gen as Gen
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
term = ask >>= \ i -> recursive (small . choice)
  ([ var_ | i > 0 ] <> [ type_, err_ ])
  ([ let_, lam_, app_, pi_, ex_, eq_ ] <*> [term])
