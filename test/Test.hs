module Main
( main
) where

import           Test.Tasty
import qualified Test.Term as Term

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Term.tests
  ]
