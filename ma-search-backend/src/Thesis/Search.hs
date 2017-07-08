-- |
-- Description: Generic search-pipeline implementation
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns#-}
module Thesis.Search where

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable)
import qualified Data.List as List
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic
import           Thesis.CodeAnalysis.Semantic.Blocks
import           Thesis.Data.Range hiding (coveragePercentage)
import           Thesis.Data.Stackoverflow.Answer (AnswerFragmentMetaData)
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.BloomFilter
import           Thesis.Search.FragmentData
import           Thesis.Search.Index
import           Thesis.Search.Levenstein
import           Thesis.Search.NGrams
import           Thesis.Search.ResultSet
import           Thesis.Search.Settings
import           Thesis.Util.VectorView
import Thesis.CodeAnalysis.Language (TokenVector)

import Debug.Trace

-- | This function serves to solve a possible case of combinatorial explosion in
-- search. The problem is the following:
--
-- Assuming there is a repeat in the query document like:
--
-- @
-- 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, 0xa39, ...
-- @
--
-- and there is a pattern that contains a similar repeat.
--
-- Search will then find all occurrences of the first part of the query-repeat
-- in the pattern - repeat. And all the identical parts of the tail of the query
-- repeat and the tail of that etc.
--
-- This number can blow up and therefore this function caps the number of times
-- that an identical n-gram is searched for.
--
removeRepeats :: (Eq t) =>
                 Int
              -> [(V.Vector (TokenWithRange t l), Int,b)]
              -> [(V.Vector (TokenWithRange t l), Int,b)]
removeRepeats n lst' = removeRepeats' S.empty n (zip lst (tail $ List.tails lst))
  where
    snd3 (_,x,_) = x
    lst = List.sortOn snd3 lst'

removeRepeats' :: (Eq t) =>
                  S.Set Int
               -> Int
               -> [((V.Vector (TokenWithRange t l), Int, b)
                   , [(V.Vector (TokenWithRange t l), Int, b)])]
               -> [(V.Vector (TokenWithRange t l), Int, b)]
removeRepeats' _ _ [] = []
removeRepeats' recognized ngramSize (((ngram, start,tl), rest):xs)
  | S.member start recognized = removeRepeats' recognized ngramSize xs
  | otherwise =
    let repeats = takeRepeats start rest
    in if length repeats > 2
       then let (ngr, start', tl') = head $ reverse repeats
                recognized' = foldl (flip S.insert) recognized (snd3 <$> repeats)
            in [(ngr, start', tl'){-, (ngram, start, tl)-}] ++
               (removeRepeats' recognized' ngramSize xs)
       else (ngram, start, tl):(removeRepeats' recognized ngramSize xs)
  where
    snd3 (_,x,_) = x
    takeRepeats _ [] = []
    takeRepeats lastRec ((ngram', start', x):rst)
     | start' > (ngramSize + lastRec) = []
     | (token <$$$> ngram) == (token <$$$> ngram') =
         (ngram',start', x):(takeRepeats start' rst)
     | otherwise = takeRepeats lastRec rst


findMatchesZero :: (Eq t, NFData t, MonadLogger m, Hashable t, FragmentData ann)
                => SearchIndex t l ann
                -> V.Vector (TokenWithRange t l) -- ^ The submitted code to be searched
                -> Int            -- ^ The minimal match length
                -> MaybeT m (ResultSet t l ann)
findMatchesZero index v n = traceShow (length lookupResults) $ return . ResultSet . (fmap (:[])) . buildMap $ do
  (qRange, fRange, md) <- lookupResults
  let textRange = 
        let (Range x y) = qRange
            !a = traceShow (qRange, fRange, V.length v) rangeStart $! coveredRange $! V.unsafeIndex v x
            !b = rangeEnd $! coveredRange $! V.unsafeIndex v (y-1)
        in Range a b
  return $! AlignmentMatch { resultQueryRange = qRange
                           , resultFragmentRange = fRange
                           , resultMetaData = md
                           , resultLevenScore = 0
                           , resultTextRange = textRange}
  where
    lookupResults = lookupZero n (token <$> v) (indexTrie index)

-- | TAKE CARE: alignment matches that are in the result set of this function
-- are not yet sorted and can cause problems with functions such as 'rangeCover'
-- or redundant match removal!
findMatches :: (NFData t, MonadLogger m, Hashable t, FragmentData ann)
               => SearchIndex t l ann
               -> Int            -- ^ The tolerated levenshtein distance
               -> TokenVector t l -- ^ The submitted code to be searched
               -> Int            -- ^ The minimal match length
               -> MaybeT m (ResultSet t l ann)
findMatches index@(SearchIndex{..}) !n !tokens !minMatchLength = do
  let ngramsWithTails = allNgramTails indexNGramSize tokens
      relevantNGramTails = filter (\(ngr, _, _) -> True ) $ --ngramRelevant ngr)
--                           filter (\(_  , x, _) -> x `mod` 5 == 0) $
                                  (removeRepeats 2 ngramsWithTails)
      resultList = searchFor <$> relevantNGramTails

  $(logDebug) $ "Number of search starting points " <>
                (Text.pack . show $ length relevantNGramTails)

  let searchResults = buildMap $ concat resultList

  searchResults `seq` $(logDebug) $ "Number of search results: " <>
                        (Text.pack . show . sum $ length <$> searchResults) <>
                        " in " <> (Text.pack . show $ M.size searchResults) <> "groups"

  return $ ResultSet $ (:[]) <$> searchResults

  where
    ngramRelevant tks = indexBF =?: (token <$> tks)

    searchFor (_, start, tokenVector) =
      let !result = search index n tokenVector minMatchLength
      in do
           (metadata, range, score) <- result
           -- TODO: instead of length foundTokens we need the length of the
           -- tokens that have been matched in the query doc!
           let !rlength = rangeLength range
           return $! AlignmentMatch
                      { resultTextRange =
                          ngramWithRange rlength (V.unsafeTake rlength tokenVector)
                      , resultQueryRange = Range start (start + rlength)
                      , resultMetaData = metadata
                      , resultFragmentRange = range
                      , resultLevenScore = score
                      }


buildMap :: (Eq ann, Hashable ann, FragmentData ann) =>
            [AlignmentMatch t l ann]
         -> M.HashMap ann [AlignmentMatch t l ann]
buildMap xs =
  M.fromListWith (++) ((\x -> (resultMetaData x, [x])) <$> xs)

{-# SPECIALIZE buildMap :: [AlignmentMatch t l (Int, Int)]
                        -> M.HashMap (Int, Int) [AlignmentMatch t l (Int, Int)]
  #-}

{-# SPECIALIZE buildMap :: [AlignmentMatch t l AnswerFragmentMetaData]
                        -> M.HashMap AnswerFragmentMetaData
                                     [AlignmentMatch t l AnswerFragmentMetaData]
  #-}

-- | For an ngram with (assumed) contiguous tokens give us the position range of
-- the whole ngram and the ngram
ngramWithRange :: Int
                  -- ^ Length of the given vector (slight performance gain)
               -> V.Vector (TokenWithRange t l)
               -> Range (LanguageText l)
ngramWithRange !n !xs =
      Range (rangeStart . coveredRange $ V.unsafeHead xs)
            (rangeEnd . coveredRange $ V.unsafeIndex xs (n - 1))
{-# INLINE ngramWithRange #-}

search :: (Hashable t, Eq t, FragmentData ann) =>
          SearchIndex t l ann
       -> Int  -- ^ Levenshtein distance
       -> V.Vector (TokenWithRange t l)
       -> Int -- ^ Minimal length of an alignment match
       -> [(ann, Range t , Int)]
search SearchIndex{..} !n !xs !minMatchLength = do
  (matchedLength, results, levenD) <- lookupAllSuff aut indexTrie minMatchLength
  (mds, dist) <- toList $ results
  md <- S.toList mds
  let rg = buildRange md matchedLength dist
  rg `seq` return $ (md, rg, levenD)
  where
    !aut = LevensteinAutomaton (V.length xs) n (token . (V.unsafeIndex xs))



tokenizeAndPerformSearch :: ( Eq t
                            , NFData t
                            , Hashable t
                            , Monad m
                            , MonadThrow m
                            , MonadLogger m
                            , FragmentData ann) =>
                            SearchIndex t l ann
                         -> Language t l
                         --              -> DataDictionary m
                         -> (ann -> MaybeT m (TokenVector t l, LanguageText l))
                         -> SearchSettings
                         -> LanguageText l
                         -> SemanticAnalyzer m a
                         -> m (Maybe (ResultSet t l ann))
tokenizeAndPerformSearch index lang dict settings txt analyzer =
  case processAndTokenize lang txt of
    Nothing -> return Nothing
    Just tks -> performSearch index lang dict settings (txt, tks) analyzer

tokenizeAndPerformCachedSearch :: ( Eq t
                            , NFData t
                            , Hashable t
                            , Monad m
                            , MonadIO m
                            , MonadThrow m
                            , MonadLogger m
                            , FragmentData ann) =>
                            SearchIndex t l ann
                         -> Language t l
                         --              -> DataDictionary m
                         -> (ann -> MaybeT m (TokenVector t l, LanguageText l))
                         -> SearchSettings
                         -> LanguageText l
                         -> SemanticAnalyzer m a
                         -> m (Maybe (ResultSet t l ann))
tokenizeAndPerformCachedSearch index lang dict settings txt analyzer =
  case processAndTokenize lang txt of
    Nothing -> return Nothing
    Just tks -> performCachedSearch index lang dict settings (txt, tks) analyzer

-- | Same as 'performSearch' but caches "dictionary" lookups if both block filtering and semantic analysis is turned on in the given search settings.
performCachedSearch :: ( Eq t
                       , NFData t
                       , Hashable t
                       , Monad m
                       , MonadIO m
                       , MonadThrow m
                       , MonadLogger m
                       , FragmentData ann) =>
                       SearchIndex t l ann
                    -> Language t l
--              -> DataDictionary m
              -> (ann -> MaybeT m (TokenVector t l, LanguageText l))
              -- ^ "Dictionary" for getting full text and tokens of fragments
              -> SearchSettings
              -> (LanguageText l, TokenVector t l)
              -> SemanticAnalyzer m a
              -> m (Maybe (ResultSet t l ann))
performCachedSearch index lang dict conf@SearchSettings{..} query analyzer
  | isJust semanticThreshold && blockFiltering =
    -- We only cache if we do both block filtering and semantic filtering
    -- because only then is caching dictionary looukps useful
    performSearch index lang dict conf query analyzer
  | otherwise = do
    $(logDebug) "Searching with cached dictionary lookups..."
    cachedDict <- cachedLookup dict
    performSearch index lang cachedDict conf query analyzer

-- | Perform a search based on a set of search settings.
--
-- Can throw a 'SemanticException' if something goes wrong in semantic processing.
performSearch :: ( Eq t
                 , NFData t
                 , Hashable t
                 , Monad m
                 , MonadThrow m
                 , MonadLogger m
                 , FragmentData ann) =>
                 SearchIndex t l ann
              -> Language t l
--              -> DataDictionary m
              -> (ann -> MaybeT m (TokenVector t l, LanguageText l))
              -> SearchSettings
              -> (LanguageText l, TokenVector t l)
              -> SemanticAnalyzer m a
              -> m (Maybe (ResultSet t l ann))
performSearch index lang dict conf@SearchSettings{..} (txt, queryTokens) analyzer = runMaybeT $ do
  $(logDebug) "Running search pipeline..."
  $(logDebug) $ "Ngram-size: " <> (Text.pack . show $ indexNGramSize index)
  $(logDebug) $ "Search-settings: " <> (Text.pack . show $ conf)
  $(logDebug) "Levenshtein - search..."

  firstMatches <- findMatchesZero index queryTokens minMatchLength

  $(logDebug) $ "Beforelength filtering there are: "
                  <> printNumberOfAlignmentMatches firstMatches
                  <> " matches in " <> printNumberOfGroups firstMatches
                  <> " groups"

  let initialMatches = sortAlignmentMatches $
                       filterSumTotalLength minSumResultLength $
--                       fragmentsLongerThan minMatchLength $
                       firstMatches


  $(logDebug) $ "After length filtering there are: "
                  <> printNumberOfAlignmentMatches initialMatches
                  <> " matches in " <> printNumberOfGroups initialMatches
                  <> " groups"

  let afterFirstCoverage = answersWithCoverage coveragePercentage initialMatches

  $(logDebug) $ "After first coverage pass: "
                  <> printNumberOfAlignmentMatches afterFirstCoverage
                  <> " matches in " <> printNumberOfGroups afterFirstCoverage
                  <> " groups"

  let nonRedundantMatches = filterSumTotalLength minSumResultLength $
                            removeSubsumptionInSet afterFirstCoverage

  $(logDebug) $ "Non redundant matches: "
                  <> printNumberOfAlignmentMatches nonRedundantMatches
                  <> " in " <> printNumberOfGroups nonRedundantMatches
                  <> " groups"

  let coverageAnalyzed = answersWithCoverage coveragePercentage nonRedundantMatches

  $(logDebug) $ "Groups after coverage / length: "
                <> printNumberOfGroups coverageAnalyzed

  $(logDebug) "Proceeding with semantic analysis"
  blockFiltered <- if blockFiltering
                   then do
                     $(logDebug) "Performing block analysis..."
                     r <- resultSetBlockAnalysis dict
                                                 lang
                                                 (token <$$$> queryTokens)
                                                 coverageAnalyzed
                     $(logDebug) "Repeat coverage filter on individual results..."
                     return $ answersWithCoverage coveragePercentage $
                              sortAlignmentMatches $
                              filterSumTotalLength minSumResultLength r
                   else do
                     $(logDebug) "Skipping block analysis."
                     MaybeT . return . Just $  coverageAnalyzed

  semanticFiltered <- case semanticThreshold of
    Nothing -> do
      $(logDebug) "Skipping word similarity analysis."
      return blockFiltered
    Just  t -> do
      $(logDebug) "Proceeding with identifier similarity analysis..."
      qData <- MaybeT $ return queryData
      res <- resultsWithSimilarity lang dict analyzer qData blockFiltered t
      return $ answersWithCoverage coveragePercentage res

  return $ semanticFiltered
  where
    queryData = do
      qt <- getQueryTokens
      return (qt, txt)
    getQueryTokens = processAndTokenize lang txt
    printNumberOfAlignmentMatches = Text.pack . show . numberOfAlignmentMatches
    printNumberOfGroups = Text.pack . show . numberOfFragments

-- | Cache the lookups of patterns in a hashmap. This helper allows us to
-- sometimes spare a database loookups if we are doing semantic simliarity
cachedLookup :: (MonadIO m, Hashable ann, Eq ann) =>
                (ann -> MaybeT m (TokenVector t l, LanguageText l))
             -> m (ann -> MaybeT m (TokenVector t l, LanguageText l))
cachedLookup doLookup = do
  mv <- liftIO $ newMVar M.empty
  return (go mv)
  where
    go mv = \x -> do
      mp <- liftIO $ takeMVar mv
      case M.lookup x mp of
        Just val -> liftIO (putMVar mv mp) >> return val
        Nothing -> do
          resultMaybe <- lift $ runMaybeT (doLookup x)
          case resultMaybe of
            Nothing -> do
              liftIO $ putMVar mv mp
              MaybeT $ return Nothing
            Just v  -> do
              liftIO (putMVar mv (M.insert x v mp))
              return v
