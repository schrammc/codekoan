{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search.CompressedTrieSpec where

import Data.List (sort, tails)
import Data.Maybe (fromMaybe)

import Test.Hspec

import Thesis.Search.CompressedTrie

import Test.Hspec.QuickCheck
import Test.QuickCheck

import Debug.Trace

import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "Thesis.CompressedTrie" $ do
    aFewWords
  describe "Thesis.CompressedTrie (suffix tries)" $ do
    allSuffixesPresent
    allMinlengthSuffixesPresent

aFewWords :: Spec
aFewWords = it "should correctly read a few fixed example words" $ do
  let tr = buildTrie ws
  (V.length $ wordsInTrie tr) `shouldBe` 3
  where
    ws = zip (V.fromList <$> ["abcdef", "abc", "abcdxxy"]) [1..]

-- | This quickcheck test ensures, that nonempty suffixes of all given words are
-- present in a suffix trie. This is a special case of
-- 'allMinlengthsuffixespresent'
allSuffixesPresent :: Spec
allSuffixesPresent =
  it "should generate suffix trees with all suffixes contained" $ do
    suffixesProperty Nothing

-- | This quickcheck test ensures, that suffix trees are constructed correctly
-- for minimum length suffixes.
allMinlengthSuffixesPresent :: Spec
allMinlengthSuffixesPresent =
  it ("should generate suffix trees with all suffixes of " ++
     "a minimum length contained") $ do
    property $ \n ->
      suffixesProperty $ if n > 0
                         then (Just n)
                         else Nothing

-- | Property that ensures, that the words in a suffix trie are equal to
-- suffixes of 'w' that are of length at least 'n'
suffixesProperty n = property $ \(w :: String) ->
  if null w
  then True
  else
    let n' = max 0 (fromMaybe 0 n)
        nonemptySuffixes = sort $ filter (\x -> length x > n') $ tails w
        w' = V.fromList w
        tr = buildSuffixTrie n w' (0 :: Int)
        trieWords = sort  $ fst <$> wordsInTrie' tr
    in trieWords == nonemptySuffixes
