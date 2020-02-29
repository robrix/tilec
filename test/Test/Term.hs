module Test.Term
( tests
) where

import Hedgehog
import Test.Gen as Gen hiding (Gen)
import Test.Tasty
import Test.Tasty.Hedgehog
import Tile.Term

tests :: TestTree
tests = testGroup "Term"
  [ testProperty "reflexivity of ==" . property $ do
    t <- forAll term
    t === t
  ]

term :: Gen (Term Int Int)
term = Gen.type'
