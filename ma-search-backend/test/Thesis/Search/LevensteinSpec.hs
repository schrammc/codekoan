{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search.LevensteinSpec where

import qualified Data.Set as S
import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Thesis.Search.CompressedTrie
import           Thesis.Search.Levenstein

spec :: SpecWith ()
spec = do
  describe "Thesis.Search.LevensteinSpec" $ do
    alwaysFindSuff

-- | Given a suffix tree is constructed for a set of nonempty
-- strings. Then each of these individual strings should be
-- retrievable from the suffix tree by the lookup function.
alwaysFindSuff :: SpecWith ()
alwaysFindSuff = do
  it "Suffixtree lookup should always find each entry" $
    -- This quickcheck property must hold for all lists of
    -- strings. Quickcheck will check this property multiple times for
    -- randomly generated lists of strings.
    property $ \(strs :: [String]) ->
      -- This logic builds a suffix tree for each of the strings and
      -- then merges these suffix trees together into a single
      -- generalized suffix tree (GST)
      let vectors = V.fromList <$> (filter (\s -> length s > 1) strs)
          -- Turn the given list of strings into a list of vectors
          -- (i.e. arrays) of characters. Then number these vectors
          -- from zero.
          indexedVectors = zip vectors (S.singleton <$> ([0..] :: [Int]))
          tries = fmap (\(tr,s) -> buildSuffixTrie Nothing tr s)
                       indexedVectors
          mergedTrie = foldl1 mergeTries tries

      -- Go over all vectors of characters and their according number,
      -- and lookup each in the merged suffix tree.
      in and $ do
        (v, i :: S.Set Int) <- indexedVectors
        let results = lookupAllSuff
                        -- A simple levenshtein automaton that only
                        -- exactly accepts it's original word.
                        (vectorToLevensteinAutomaton 0 v)
                        -- Run on the whole merged trie.
                        mergedTrie
                        -- Minimal match length is set to zero.
                        0
            resultSets = do
              (_, setsAndPositions, _) <- results
              (set, _) <- setsAndPositions
              return set
        -- Make sure that the original vector is in the results
        return $ i `elem` resultSets


