-- | This module contains code for searching in given indices

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search where

import           Data.Hashable (Hashable)
import qualified Data.Set as S
import qualified Data.Vector as V

import           Data.Maybe(fromJust)

import           Control.Parallel.Strategies

import           Thesis.CodeAnalysis.Language
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Text.PositionRange
import           Thesis.Data.Range
import           Thesis.Search.BloomFilter
import           Thesis.Search.Levenstein
import           Thesis.Search.NGrams
import           Thesis.Search.Index
import           Thesis.Search.SearchResult
import           Thesis.Search.ResultSet

findMatches :: (Ord t, Hashable t)
               => SearchIndex t l
               -> Int            -- ^ The tolerated levenshtein distance
               -> LanguageText l -- ^ The submitted code to be searched
               -> Maybe (ResultSet t)
findMatches index@(SearchIndex{..}) n t = do
  tokens <- maybeTokens
  let ngramsWithTails = allNgramTails indexNGramSize tokens
      relevantNGramTails = filter (ngramRelevant . fst) ngramsWithTails
      relevantTails = snd <$> relevantNGramTails
      -- parMap use here is probably not yet optimal
      searchResults = concat $ parMap rpar searchFor relevantTails
  return $ removeSubsumption $ buildResultSet searchResults

  where
    maybeTokens = processAndTokenize indexLanguage t
    ngramRelevant tks = indexBF =?: (snd <$> tks)

    searchFor ts  = 
      let tokenVector = V.fromList $ snd <$> ts
          result = search index n tokenVector
      in do
           (foundTokens, metadata, range, score) <- result
           case ngramWithRange (take (length foundTokens) ts) of
             Nothing -> []
             Just x -> return SearchResult { resultTextRange = fst x
                                           , resultMatchedTokens = foundTokens
                                           , resultMetaData = metadata
                                           , resultFragmentRange = range
                                           , resultLevenScore = score
                                           }

-- | A range starting at the start of the first range and ending at the end of
-- the second range
mergePositionRanges :: Range -> Range -> Range
mergePositionRanges (Range start _) (Range _ end) =
                        Range start end

-- | For an ngram with (assumed) contiguous tokens give us the position range of
-- the whole ngram and the ngram
ngramWithRange :: [(Range, t)] -> Maybe (Range, [t])
ngramWithRange [] = Nothing
ngramWithRange xs = let (rs, ts) = unzip xs
                    in Just (foldl1 mergePositionRanges rs, ts)

search :: (Ord t) => SearchIndex t l
       -> Int  -- ^ Levenshtein distance
       -> V.Vector t
       -> [([t], AnswerFragmentMetaData, Range , Int)]
search SearchIndex{..} n xs = do
  (tks, results, levenD) <- lookupAllSuff aut indexTrie
  (mds, dist) <- results
  md <- S.toList mds
  let rg = buildRange md tks dist
  return (tks, md, rg, levenD)
  where
    aut = LevensteinAutomaton (V.length xs) n (xs V.!)

-- | Given an answer sequence, a sequence of matched tokens and a remainder
-- return the range of covered tokens in the answer fragments.
buildRange :: AnswerFragmentMetaData -> [t] -> Int -> Range
buildRange AnswerFragmentMetaData{..} tks d =
  Range (fragmentMetaSize - (n + d)) (fragmentMetaSize - d)
  where
    n = length tks
    
