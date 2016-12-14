{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search.ResultSet ( ResultSet(..)
                               , listOfResults
                               , buildResultSet
                               , mapFragmentResults
                               , mapSplitFragmentResults
                               , answersWithCoverage
                               , fragmentsLongerThan
                               -- * SummaryInformation
                               , numberOfAnswers
                               , numberOfFragments
                               , numberOfGroups

                               , filterEmptyResults
                                 -- * Helper functions
                               , flattenSet
                               , buildSet
                               )where

import           Data.List (groupBy)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M

import           Thesis.Data.Range
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.FragmentData

-- | Search results organized into questions and fragments of these questions
newtype ResultSet t l ann =
  ResultSet {resultSetMap :: M.Map ann [[AlignmentMatch t l ann]]}

-- | This function makes sure, that for each answer there is at least one
-- fragment, that contains at least one search results.
filterEmptyResults :: (Ord ann) => ResultSet t l ann -> ResultSet t l ann
filterEmptyResults ResultSet{..} = ResultSet . M.fromList $ do
  (ann, results) <- M.toList resultSetMap

  let nonemptySearchResults = filter (not . null) results
  if not $ null nonemptySearchResults
     then return (ann, nonemptySearchResults)
     else []

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
  mapFragmentResults resultSet $ \_ -> \rs@(r:_) -> 
     let n = fragDataTokenLength $ resultMetaData r
         frags = resultFragmentRange <$> rs
     in if coveragePercentage n frags >= cov'
        then Just rs
        else Nothing
  where
    cov' = max 0 (min 1 cov)

fragmentsLongerThan :: (Eq t)
                       => Int -- ^ Minimum length of an answer fragment
                       -> ResultSet t l ann
                       -> ResultSet t l ann
fragmentsLongerThan n resultSet =
  mapFragmentResults resultSet $ \_ -> \results -> 
    case filter (\r -> length (resultMatchedTokens r) >= n) results of
      [] -> Nothing
      rs -> Just rs

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
      rs  -> Just rs


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


-- | Build a result set from a list of alignment matches in which there is no
-- alignment match for an answer is subsumed by another.
buildResultSet :: (Eq t, Ord ann) => [AlignmentMatch t l ann] -> ResultSet t l ann
buildResultSet  = removeSubsumptionInSet . buildResultSet'

-- | Build a result set from a list of search results.
buildResultSet' :: (Eq t, Ord ann) => [AlignmentMatch t l ann] -> ResultSet t l ann
buildResultSet' [] = ResultSet M.empty
buildResultSet' results =
  let groupedByAnn = groupBy (\a b -> resultMetaData a == resultMetaData b) results
  in ResultSet $ foldl1 (M.unionWith (++)) $ do
    annGroup <- groupedByAnn
    let ann = resultMetaData $ head annGroup
    return $ M.singleton ann  [annGroup]


removeSubsumptionInSet:: (Eq t, Eq ann) => ResultSet t l ann -> ResultSet t l ann
removeSubsumptionInSet ResultSet{..}  =
  ResultSet $  fmap (\rs -> [removeSubsumption $ concat rs]) resultSetMap

-- TODO: Update comment
-- | For each fragment remove all search results, that are properly subsumed by
-- another search result. Note that this will not remove all answer fragments
-- from a search result set, as there is always at least one search result per
-- answer fragment, that is not subsumed by another.
removeSubsumption :: (Eq t, Eq ann) => [AlignmentMatch t l ann] -> [AlignmentMatch t l ann]
removeSubsumption results = do
      r <- results
      if null $ filter (/= r) $ filter (subsumedByProper r) results
        then [r]
        else []


-- | Get the number of answers for which this result set contains alignment
-- match groups.
numberOfAnswers :: ResultSet t l ann -> Int
numberOfAnswers ResultSet{..} = M.size resultSetMap

-- |
numberOfFragments :: ResultSet t l ann -> Int
numberOfFragments ResultSet{..} = sum $ do
  (_, frags) <- M.toList resultSetMap
  return $ length frags

numberOfGroups :: ResultSet t l ann -> Int
numberOfGroups ResultSet{..} = sum $ do
  (_, groupList) <- M.toList resultSetMap
  return $ length groupList

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
buildSet :: Ord ann => [(ann, [AlignmentMatch t l ann])] -> ResultSet t l ann
buildSet lst = ResultSet $ M.fromList fragGroups
  where
    fragGroups = mergeGroup <$> (groupBy (\(ann, _) (ann', _) -> ann == ann') lst)

    mergeGroup :: [(ann, [AlignmentMatch t l ann])]
               -> (ann, [[AlignmentMatch t l ann]])
    mergeGroup [] = error "Thesis.CodeAnalysis.Semantic - impossible case"
    mergeGroup xs@((ann, _):_) = (ann, snd <$> xs)
