module Thesis.Search.CompressedTrieSpec where

import Test.Hspec

import Thesis.Search.CompressedTrie

import Data.Vector as V

spec = do
  describe "Thesis.CompressedTrie" $ do
    aFewWords
      
aFewWords = it "should correctly read a few words" $ do
  let tr = buildTrie [("abcdef",1), ("abc",2),("abcdxxy",3)]
  print tr
  (V.length $ wordsInTrie tr) `shouldBe` 3
