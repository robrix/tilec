module Test.Term
( tests
) where

import Control.Monad.Trans.Reader
import Hedgehog hiding (Gen)
import Hedgehog.Gen as Gen
import Test.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Tile.Term

tests :: TestTree
tests = testGroup "Term"
  [ testProperty "reflexivity of ==" . property $ do
    t <- forAll (runReaderT (runGen term) 0)
    t === t
  ]

term :: Gen (Term Int Int)
term = runReaderT go (0 :: Int) where
  go = ask >>= \ i -> recursive choice
    (if i > 0 then [ Gen.localVar, Gen.type' ] else [ Gen.type' ])
    [ Gen.subterm2 go (local succ go) (\ t b -> let' t (const b))
    ]
