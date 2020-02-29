{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Term
( tests
) where

import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Set
import Hedgehog as H
import Hedgehog.Gen as Gen
import Test.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Tile.Term

tests :: TestTree
tests = testGroup "Term"
  [ testProperty "reflexivity of ==" . property $ do
    (labels, t) <- forAll (runReaderT term 0)
    for_ labels label
    t H.=== t
  ]

term :: ReaderT Int Gen (Set LabelName, Term Int Int)
term = go where
  go = ask >>= \ i -> recursive choice
    ((if i > 0 then (tag "var" Gen.localVar :) else id) [ tag "type" (pure type') ])
    [ tag "let" $ Gen.subterm2 go' (local succ go') (\ t b -> let' t (const b))
    , tag "lam" $ Gen.subtermM (local succ go') (\ b -> lam <$> plicit <*> pure (const b))
    , tag "$$"  $ Gen.subterm2 go' go' ($$)
    , tag ">->" $ Gen.subtermM2 go' (local succ go') (\ t b -> (>-> const b) . (, t) <$> plicit)
    , tag "ex"  $ Gen.subterm2 go' (local succ go') (\ t b -> t `ex` const b)
    ]
  go' = snd <$> go
  tag s = fmap (singleton s,)
