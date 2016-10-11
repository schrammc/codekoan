module Thesis.Search.LevensteinSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Set as S
import qualified Data.Vector as V

import Thesis.Search.Levenstein
import Thesis.Search.CompressedTrie
import Debug.Trace
spec = do
  describe "Thesis.Search.LevensteinSpec" $ do
    lookupSimple

lookupSimple = it "example 1" $ lookupAllSuff aut tr `shouldBe` [("abcdef",[(S.singleton 1, 0)],0)]
  where
    tr = foldl mergeTries empty $ do
      (w, v) <- ws
      [buildSuffixTrie Nothing w v]
    ws = zip (V.fromList <$> ["abcdef", "abc", "abcdxxy"]) (S.singleton <$> [1..])
    wordV = V.fromList "abcd"
    aut = LevensteinAutomaton (V.length wordV) 0 (V.unsafeIndex wordV)
