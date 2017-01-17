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

-- | Given a suffix tree is constructed for a set of nonempty strings. Then each
-- of these individual strings should be retrievable from the suffix tree by the
-- lookup function.
alwaysFindSuff :: Spec
alwaysFindSuff = do
  it "Suffixtree lookup should always find each entry" $
    property $ \(strs :: [String]) ->
      let vectors = V.fromList <$> (filter (\s -> length s > 1) strs)
          indexedVectors = zip vectors (S.singleton <$> ([0..] :: [Int]))
          tries = (\(tr,s) -> buildSuffixTrie Nothing tr s) <$> indexedVectors
          mergedTrie = foldl1 mergeTries tries
      in and $ do
        (v, i :: S.Set Int) <- indexedVectors
        let results = lookupAllSuff (vectorToLevensteinAutomaton 0 v) mergedTrie 0
            resultSets = do
              (_, setsAndPoss, _) <- results
              (set, _) <- setsAndPoss
              return set
        return $ i `elem` resultSets

