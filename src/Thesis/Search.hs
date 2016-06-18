-- | This module contains code for searching in given indices

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search where

import qualified Data.Vector as V

import           Debug.Trace

import           Data.Hashable (Hashable)

import           Thesis.CodeAnalysis.Tokenizer
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Text.PositionRange
import           Thesis.Search.BloomFilter
import           Thesis.Search.Levenstein
import           Thesis.Search.NGrams
import           Thesis.Search.Index

findMatches :: (Ord t, Hashable t)
               => SearchIndex t l 
               -> Int             -- ^ The tolerated levenstein distance
               -> LanguageText l  -- ^ The submitted code to be searched
               -> Maybe [(PositionRange, [t], AnswerId, Int)]
findMatches index@(SearchIndex{..}) n t = do
  tokens <- maybeTokens
  let ngramsWithTails = allNgramTails indexNGramSize tokens
      relevantNGramTails = let before = length ngramsWithTails
                               result = filter (ngramRelevant . fst) ngramsWithTails
                               after = length result
                           in traceShow (fromIntegral after / (fromIntegral before :: Double))  result
      relevantTails = snd <$> relevantNGramTails
  return (concat $ searchFor <$> relevantTails)
  where
    maybeTokens = processAndTokenize indexLanguage t
    ngramRelevant tks = indexBF =?: (snd <$> tks)

    searchFor ts  = 
      let tokenVector = V.fromList $ snd <$> ts
          result = search index n tokenVector
      in do
           (foundTokens, aId, score) <- result
           case ngramWithRange (take (length foundTokens) ts) of
             Nothing -> []
             Just x -> return (fst x, foundTokens, aId, score)

-- | A range starting at the start of the first range and ending at the end of
-- the second range
mergePositionRanges :: PositionRange -> PositionRange -> PositionRange
mergePositionRanges (PositionRange start _) (PositionRange _ end) =
                        PositionRange start end

-- | For an ngram with (assumed) contiguous tokens give us the position range of
-- the whole ngram and the ngram
ngramWithRange :: [(PositionRange, t)] -> Maybe (PositionRange, [t])
ngramWithRange [] = Nothing
ngramWithRange xs = let (rs, ts) = unzip xs
                    in Just (foldl1 mergePositionRanges rs, ts)

search :: (Ord t) => SearchIndex t l -> Int -> V.Vector t -> [([t],AnswerId, Int)]
search SearchIndex{..} n xs =
  lookupAllL aut indexTrie
  where
    aut = LevensteinAutomaton (V.length xs) n (xs V.!)

