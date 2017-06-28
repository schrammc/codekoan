-- |
-- Description: Search pipeline result set
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Thesis.Search.ResultSet ( ResultSet(..)
                               , listOfResults
                               , buildResultSet
                               , mapFragmentResults
                               , mapSplitFragmentResults
                               , answersWithCoverage
                               , fragmentsLongerThan
                               , filterSumTotalLength
                               -- * SummaryInformation
                               , numberOfAnswers
                               , numberOfFragments
                               , numberOfAlignmentMatches

                               , filterEmptyResults
                                 -- * Helper functions
                               , flattenSet
                               , buildSet
                               , removeSubsumptionInSet

                               , removeSubsumption'
                               , sortAlignmentMatches
                               )where

import           Control.DeepSeq
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.List (groupBy, sortOn, sortBy, foldl')
import           Data.Maybe (catMaybes)
import           Thesis.Data.Range
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.FragmentData
import Data.Ord

-- | Search results organized into questions and fragments of these questions
newtype ResultSet t l ann =
  ResultSet {resultSetMap :: M.HashMap ann [[AlignmentMatch t l ann]]}
  deriving (Show, NFData)

-- | This function makes sure, that for each answer there is at least one
-- fragment, that contains at least one search results.
filterEmptyResults :: (Eq ann, Hashable ann) =>
                      ResultSet t l ann
                   -> ResultSet t l ann
filterEmptyResults ResultSet{..} = ResultSet $ M.mapMaybe f resultSetMap
  where
    f results = 
      let nonemptySearchResults = filter (not . null) results
      in if not $ null nonemptySearchResults
         then Just nonemptySearchResults
         else Nothing

listOfResults :: ResultSet t l ann -> [AlignmentMatch t l ann]
listOfResults (ResultSet mp) = do
  (_, res) <- M.toList mp
  concat res

-- | Filter out fragments from a result set, that are below a certain fraction
-- of coverage. If no fragment for an answer has sufficient coverage, we
-- filter out the answer.
answersWithCoverage :: (Eq t, FragmentData ann)
                        => Double -- ^ A fraction (>= 0 and <= 1)
                        -> ResultSet t l ann
                        -> ResultSet t l ann
answersWithCoverage cov resultSet =
  mapFragmentResults resultSet $ \ann -> \rs -> 
     let !n = fragDataTokenLength ann
         !frags = resultFragmentRange <$> rs
     in if coveragePercentage n frags >= cov'
        then Just rs
        else Nothing
  where
    !cov' = max 0 (min 1 cov)

-- | Filters out individual alignment matches that are shorter than the given
-- minimum length.
fragmentsLongerThan :: (Eq t)
                       => Int -- ^ Minimum length of an answer fragment
                       -> ResultSet t l ann
                       -> ResultSet t l ann
fragmentsLongerThan !n resultSet =
  mapFragmentResults resultSet $ \_ -> \results -> 
    case filter (\r -> rangeLength (resultFragmentRange r) >= n) results of
      [] -> Nothing
      rs -> length rs `seq` Just rs

-- | Map a function over all fragment result groups in a result set. If the
-- given function returns 'Nothing' then the fragment is removed from the result
-- set. If an answer contains no more fragments after application of the
-- function, then the answer is removed as well. The function is guaranteed to
-- never be given an empty list of search results as an argument.
mapFragmentResults :: ResultSet t l ann
                   -> (ann
                       -> [AlignmentMatch t l ann]
                       -> Maybe [AlignmentMatch t l ann])
                   -> ResultSet t l ann
mapFragmentResults ResultSet{..} f = ResultSet $ 
  (flip M.mapMaybeWithKey) resultSetMap $ \ann -> \results ->
    case catMaybes $ f ann <$> results  of
      [] -> Nothing
      rs  -> length rs `seq` Just rs

-- | Map a function over all fragment result groups in a result set. If the
-- given function returns an empty set of result groups then remove the result
-- groups for that answer fragment from the result set entirely.
mapSplitFragmentResults :: ResultSet t l ann
                        -> (ann 
                            -> [AlignmentMatch t l ann]
                            -> [[AlignmentMatch t l ann]])
                        -> ResultSet t l ann
mapSplitFragmentResults ResultSet{..} f = ResultSet $ 
  (flip M.mapMaybeWithKey) resultSetMap $ \ann -> \results ->
    case filter (not . null) $ results >>= f ann of
      [] -> Nothing
      rs  -> Just rs

-- | Build a result set from a list of search results.
buildResultSet :: (Eq t, Hashable ann, Ord ann) =>
                  [AlignmentMatch t l ann]
               -> ResultSet t l ann
buildResultSet [] = ResultSet M.empty
buildResultSet results =
  ResultSet $ M.map (\x -> [x]) $ foldl combine M.empty results
  where
    f match Nothing = Just [match]
    f match (Just ms) = Just $ match:ms
    combine mp match = M.alter (f match) (resultMetaData match) mp

-- | Ensure that a result set is maximal with respect to subsumption, i.e. that
-- no alignment match for a pattern subsumes another.
removeSubsumptionInSet :: (Eq t, Eq ann) => ResultSet t l ann -> ResultSet t l ann
removeSubsumptionInSet ResultSet{..}  =
  ResultSet $  fmap (\rs -> removeSubsumption' <$> rs) resultSetMap

sortAlignmentMatches :: ResultSet t l ann -> ResultSet t l ann
sortAlignmentMatches rs = mapFragmentResults rs $ \_ matches ->
  Just $ sortBy comp matches
  where
    comp ra rb =
      case compare (rangeStart $ resultQueryRange ra)
                   (rangeStart $ resultQueryRange rb) of
        EQ -> compare ((-1) * (rangeLength $ resultQueryRange ra))
                      ((-1) * (rangeLength $ resultQueryRange rb))
        x  -> x

removeSubsumption' :: (Eq t, Eq ann) =>
                      [AlignmentMatch t l ann]
                   -> [AlignmentMatch t l ann]
removeSubsumption' [] = []
removeSubsumption' results = maxSet [] results
  where
    maxSet _      []        = []
    maxSet active (next:xs) =
      let (# subsumedByNone, active' #) = adjustActive active next
      in if subsumedByNone
         then next:(maxSet active' xs)
         else maxSet active' xs
    adjustActive []     next = (# True, [next] #)
    adjustActive active next =
      let active' = dropWhile (\a -> (rangeEnd $ resultQueryRange a) <
                                     (rangeStart $ resultQueryRange next))
                    active
          subsumedByNone = null $
                           filter (subsumedProperSame next) active'
      in if subsumedByNone
         then (# True, next:active' #)
         else (# False, active' #)

-- | Get the number of answers for which this result set contains alignment
-- match groups.
numberOfAnswers :: ResultSet t l ann -> Int
numberOfAnswers ResultSet{..} = M.size resultSetMap

-- |
numberOfFragments :: ResultSet t l ann -> Int
numberOfFragments ResultSet{..} = sum $ do
  (_, frags) <- M.toList resultSetMap
  return $ length frags

numberOfAlignmentMatches :: ResultSet t l ann -> Int
numberOfAlignmentMatches ResultSet{..} = foldl' (+) 0 $ do
  (_, groupList) <- M.toList resultSetMap
  group <- groupList
  return $! length group

-- | A helper function to flatten a result set into a list
flattenSet :: ResultSet t l ann -> [(ann, [AlignmentMatch t l ann])]
flattenSet ResultSet{..} = do
  (ann, matches) <- M.toList resultSetMap
  match <- matches
  return (ann, match)

-- | A helper function to build a result set from a list, it should hold that
-- @
-- (buildSet . flattenSet) s == s
-- @
buildSet :: (Eq ann, Hashable ann) =>
            [(ann, [AlignmentMatch t l ann])]
         -> ResultSet t l ann
buildSet lst = ResultSet $ M.fromList fragGroups
  where
    fragGroups = mergeGroup <$> (groupBy (\(ann, _) (ann', _) -> ann == ann') lst)

    mergeGroup :: [(ann, [AlignmentMatch t l ann])]
               -> (ann, [[AlignmentMatch t l ann]])
    mergeGroup [] = error "Thesis.CodeAnalysis.Semantic - impossible case"
    mergeGroup xs@((ann, _):_) = (ann, snd <$> xs)

-- | Filters out groups of alignment matches for which the sum of the query
-- token ranges is shorter than the given minimum amount.
filterSumTotalLength :: Int
                        -- ^ Minimum sum of query token ranges in remaining
                        -- results
                     -> ResultSet t l ann
                     -> ResultSet t l ann
filterSumTotalLength n resultSet =
  mapFragmentResults resultSet $ \_ results ->
    if totalLength results >= n
    then Just results
    else Nothing
  where
    -- The length of all query token ranges summed up
    totalLength :: [AlignmentMatch t l ann] -> Int
    totalLength xs = foldl' (+) 0 (rangeLength . resultFragmentRange <$> xs)
