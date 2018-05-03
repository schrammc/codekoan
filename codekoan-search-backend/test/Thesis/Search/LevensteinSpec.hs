{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search.LevensteinSpec where

import Data.Foldable (toList)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Thesis.Search.CompressedTrie
import           Thesis.Search.Levenstein
import Thesis.Data.Range
import qualified Data.Map as M
import Debug.Trace

spec :: SpecWith ()
spec = do
  describe "Thesis.Search.LevensteinSpec" $ do
    alwaysFindSuff
    findDuplicatePatterns
    sameZeroLookup
    alwaysFindZero

sameZeroLookup :: SpecWith ()
sameZeroLookup =
  it "ZeroLookup" $
    property $ \(q :: [Int], fs :: [[Int]]) ->
      if length q == 0
      then True
      else 
        let results = lookupZero 0
                                 (V.fromList q)
                                 (tr (V.fromList q) (V.fromList <$> fs))
        in if contains q results
           then True
           else traceShow (q, fs, (\(BasicResult a _ _) -> a) <$> results) False
  where
    tr q fs = foldl1 (mergeTriesWith (S.union)) $ do
      (n, f) <- (zip ([0..] :: [Int]) (q:fs))
      return $ buildSuffixTrie Nothing f (S.singleton (n, V.length f))
    contains _ [] = False
    contains q ((BasicResult rq _ x):xs)
      | rq == Range 0 (length q) && x == (0, length q) = True
      | otherwise = contains q xs

alwaysFindZero = do
  it "Suffixtree lookup with triesearch should find each entry" $
    property $ \(strs' :: [String]) ->
      let strs = filter (not . null) strs'
      in case strs of
        [] -> True
        _ -> 
          let vectors = V.fromList <$> strs
              trie = foldl1 (mergeTriesWith S.union) $ do
                (n, v) <- zip ([0 ..] :: [Int]) vectors
                return $ buildSuffixTrie Nothing v (S.singleton (n, V.length v))
          in and $ do
             (n, v) <- zip ([0..] :: [Int]) vectors
             let f (BasicResult a _ b) = (a,b)
                 lookupRes = (f <$> lookupZero 0 v trie)
             k <- [0.. (V.length v - 1)]
             let x = (Range k (V.length v), (n, V.length v))
             return $ x `elem` lookupRes

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
          mergedTrie = foldl1 (mergeTriesWith S.union) tries

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
              (set, _) <- toList setsAndPositions
              return set
        -- Make sure that the original vector is in the results
        return $ i `elem` resultSets

-- | This is a very basic quickcheck property that only says, that
-- levenshtein-search with distance 0 should always find a string n-times if the
-- string is given n-times in the suffix tree.
findDuplicatePatterns :: SpecWith ()
findDuplicatePatterns = do
  it "All instance of an identical indexed string should be found" $
    property $ \(n :: Int, str :: String) -> True
      --if length str < 1 || n < 1
      --then True
      --else let charV = V.fromList str
      --         charVectors = take n $ repeat charV
      --         indexedVectors = zip charVectors (S.singleton <$> ([0..] :: [Int]))
      --         tries = fmap (\(tr, s) -> buildSuffixTrie Nothing tr s)
      --                      indexedVectors
      --         mergedTrie = foldl1 (mergeTriesWith S.union) tries
      --         xs = M.fromList $ do
      --           (key,val,_) <- toList $
      --                            lookupAllSuff (vectorToLevensteinAutomaton 0 charV)
      --                                          mergedTrie
      --                                          0
      --           return (key, toList val)
      --         -- We are not interested in any partial findings, therefore we
      --         -- only look for the located complete results containing 'str'
      --         -- whole.
      --         Just ((k, _):[]) = M.lookup str xs
      --     in k == n
