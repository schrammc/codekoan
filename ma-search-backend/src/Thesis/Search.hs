-- | This module contains code for searching in given indices

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.Search where

import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Control.Parallel.Strategies
import           Control.Monad.Logger
import           Data.Hashable (Hashable)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as Text
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic
import           Thesis.Data.Range hiding (coveragePercentage)
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.BloomFilter
import           Thesis.Search.Index
import           Thesis.Search.Levenstein
import           Thesis.Search.NGrams
import           Thesis.Search.ResultSet
import           Thesis.Search.Settings
import           Thesis.CodeAnalysis.Semantic.Blocks

findMatches :: (Ord t, Hashable t)
               => SearchIndex t l
               -> Int            -- ^ The tolerated levenshtein distance
               -> LanguageText l -- ^ The submitted code to be searched
               -> Int            -- ^ The minimal match length
               -> Maybe (ResultSet t l)
findMatches index@(SearchIndex{..}) n t minMatchLength = do
  tokens <- maybeTokens
  let ngramsWithTails = allNgramTails indexNGramSize tokens
      relevantNGramTails = filter (\(ngr, _, _) -> ngramRelevant ngr)
                                  ngramsWithTails
      relevantTails = (\(_, start, rest) -> (start, rest)) <$> relevantNGramTails
      -- parMap use here is probably not yet optimal
      searchResults = concat $ parMap rpar searchFor relevantTails
  return $ buildResultSet searchResults

  where
    maybeTokens = processAndTokenize indexLanguage t
    ngramRelevant tks = indexBF =?: (token <$> tks)

    searchFor (start, ts)  = 
      let tokenVector = V.fromList $ token <$> ts
          result = search index n tokenVector minMatchLength
      in do
           (foundTokens, metadata, range, score) <- result
           -- TODO: instead of length foundTokens we need the length of the
           -- tokens that have been matched in the query doc!
           let queryRange = Range start (start + length foundTokens)
           case ngramWithRange (take (length foundTokens) ts) of
             Nothing -> []
             Just x -> return AlignmentMatch { resultTextRange = fst x
                                             , resultMatchedTokens = foundTokens
                                             , resultQueryRange = queryRange
                                             , resultMetaData = metadata
                                             , resultFragmentRange = range
                                             , resultLevenScore = score
                                             }

-- | A range starting at the start of the first range and ending at the end of
-- the second range
mergePositionRanges :: Range a -> Range a -> Range a
mergePositionRanges (Range start _) (Range _ end) =
                        Range start end

-- | For an ngram with (assumed) contiguous tokens give us the position range of
-- the whole ngram and the ngram
ngramWithRange :: [TokenWithRange t l] -> Maybe (Range (LanguageText l), [t])
ngramWithRange [] = Nothing
ngramWithRange xs = let (rs, ts) = unzip $ (\(TokenWithRange r t) -> (r,t)) <$> xs
                    in Just (foldl1 mergePositionRanges rs, ts)

search :: (Ord t) => SearchIndex t l
       -> Int  -- ^ Levenshtein distance
       -> V.Vector t
       -> Int -- ^ Minimal length of an alignment match
       -> [([t], AnswerFragmentMetaData, Range t , Int)]
search SearchIndex{..} n xs minMatchLength = do
  (tks, results, levenD) <- lookupAllSuff aut indexTrie minMatchLength
  (mds, dist) <- results
  md <- S.toList mds
  let rg = buildRange md tks dist
  return (tks, md, rg, levenD)
  where
    aut = LevensteinAutomaton (V.length xs) n (xs V.!)

-- | Given an answer sequence, a sequence of matched tokens and a remainder
-- return the range of covered tokens in the answer fragments.
buildRange :: AnswerFragmentMetaData -> [t] -> Int -> Range a
buildRange AnswerFragmentMetaData{..} tks d =
  Range (fragmentMetaSize - (n + d)) (fragmentMetaSize - d)
  where
    n = length tks
    

-- | Perform a search based on a set of search settings.
--
performSearch :: (Hashable t, Ord t, Monad m, MonadThrow m, MonadLogger m) =>
                 SearchIndex t l
              -> Language t l
              -> DataDictionary m
              -> SearchSettings
              -> LanguageText l
              -> SemanticAnalyzer m a
              -> m (Maybe (ResultSet t l))
performSearch index lang dict SearchSettings{..} txt analyzer = runMaybeT $ do
  $(logDebug) "Running search pipeline..."
  $(logDebug) "Levenshtein - search..."

  initialMatches <- MaybeT . return $ findMatches index
                                                  levenshteinDistance
                                                  txt
                                                  minMatchLength

  $(logDebug) $ "Initial fragments: " <> printNumberOfFrags initialMatches

  let minLengthMatches = fragmentsLongerThan minMatchLength initialMatches
      coverageAnalyzed = answersWithCoverage coveragePercentage minLengthMatches

  $(logDebug) $ "After coverage / length: " <> printNumberOfFrags coverageAnalyzed

  queryTokens <- MaybeT . return $ getQueryTokens

  $(logDebug) "Proceeding with semantic analysis"
  blockFiltered <- if blockFiltering
                   then do
                     $(logDebug) "Performing block analysis..."
                     r <- resultSetBlockAnalysis dict
                                                 lang
                                                 (token <$> queryTokens)
                                                 coverageAnalyzed
                     return $ answersWithCoverage coveragePercentage r
                   else do
                     $(logDebug) "Skipping block analysis."
                     MaybeT . return . Just $  coverageAnalyzed
  semanticFiltered <- case semanticThreshold of
    Nothing -> do
      $(logDebug) "Skipping word similarity analysis."
      return blockFiltered
    Just  t -> do
      $(logDebug) "Proceeding with similarity analysis..."
      qData <- MaybeT $ return queryData
      res <- resultsWithSimilarity lang dict analyzer qData blockFiltered t
      return $ answersWithCoverage coveragePercentage res

  return $ semanticFiltered
  where
    queryData = do
      qt <- getQueryTokens
      return (qt, normalize lang txt)
    getQueryTokens = processAndTokenize lang txt
    printNumberOfFrags = Text.pack . show . numberOfFragments

