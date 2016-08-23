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
                               )where

import           Data.List (groupBy)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M

import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.SearchResult

-- | Search results organized into questions and fragments of these questions
newtype ResultSet t l =
  ResultSet {resultSetMap :: (M.Map AnswerId (M.Map Int [[SearchResult t l]]))}

-- | This function makes sure, that for each answer there is at least one
-- fragment, that contains at least one search results.
filterEmptyResults :: ResultSet t l -> ResultSet t l
filterEmptyResults ResultSet{..} = ResultSet . M.fromList $ do
  (aId, mp) <- filter (\(_ , mp) -> not $ M.null mp) (M.toList resultSetMap)
  let mp' = M.fromList $ do
        (fragId, results) <- M.toList mp
        let nonemptySearchResults = filter (not . null) results
        if not $ null nonemptySearchResults
          then return (fragId, nonemptySearchResults)
          else []
        
  return (aId, mp')

listOfResults :: ResultSet t l -> [SearchResult t l]
listOfResults (ResultSet mp) = do
  (_, mp') <- M.toList mp
  (_, res) <- M.toList mp'
  concat res

-- | Filter out fragments from a result set, that are below a certain fraction
-- of coverage. If no fragment for an answer has sufficient coverage, we
-- filter out the answer.
answersWithCoverage :: (Eq t)
                        => Double -- ^ A fraction (>= 0 and <= 1)
                        -> ResultSet t l
                        -> ResultSet t l
answersWithCoverage cov resultSet =
  mapFragmentResults resultSet $ \_ -> \_ -> \rs@(r:_) -> 
     let n = fragmentMetaSize $ resultMetaData r
         frags = resultFragmentRange <$> rs
     in if coveragePercentage n frags >= cov'
        then Just rs
        else Nothing
  where
    cov' = max 0 (min 1 cov)

fragmentsLongerThan :: (Eq t)
                       => Int -- ^ Minimum length of an answer fragment
                       -> ResultSet t l
                       -> ResultSet t l
fragmentsLongerThan n resultSet =
  mapFragmentResults resultSet $ \_ -> \_ -> \results -> 
    case filter (\r -> length (resultMatchedTokens r) >= n) results of
      [] -> Nothing
      rs -> Just rs

-- | Map a function over all fragment result groups in a result set. If the
-- given function returns 'Nothing' then the fragment is removed from the result
-- set. If an answer contains no more fragments after application of the
-- function, then the answer is removed as well. The function is guaranteed to
-- never be given an empty list of search results as an argument.
mapFragmentResults :: ResultSet t l
                   -> (AnswerId
                       -> Int
                       -> [SearchResult t l]
                       -> Maybe [SearchResult t l])
                   -> ResultSet t l
mapFragmentResults ResultSet{..} f = ResultSet $ 
  (flip M.mapMaybeWithKey) resultSetMap $ \aId -> \mp ->
    let mp' = (flip M.mapMaybeWithKey) mp $ \fragId -> \results ->
          case catMaybes $ f aId fragId <$> results  of
            [] -> Nothing
            rs  -> Just rs
    in if M.null mp'
       then Nothing
       else Just mp'

-- | Map a function over all fragment result groups in a result set. If the
-- given function returns an empty set of result groups then remove the result
-- groups for that answer fragment from the result set entirely.
mapSplitFragmentResults :: ResultSet t l
                        -> (AnswerId
                            -> Int
                            -> [SearchResult t l]
                            -> [[SearchResult t l]])
                        -> ResultSet t l
mapSplitFragmentResults ResultSet{..} f = ResultSet $ 
  (flip M.mapMaybeWithKey) resultSetMap $ \aId -> \mp ->
    let mp' = (flip M.mapMaybeWithKey) mp $ \fragId -> \results ->
          case filter (not . null) $ results >>= f aId fragId   of
            [] -> Nothing
            rs  -> Just rs
    in if M.null mp'
       then Nothing
       else Just mp'

-- | Build a result set from a list of search result in which there is no
-- alignment result for an answer is subsumed by another.
buildResultSet :: Eq t => [SearchResult t l] -> ResultSet t l
buildResultSet  = removeSubsumptionInSet . buildResultSet'

-- | Build a result set from a list of search results.
buildResultSet' :: (Eq t) => [SearchResult t l] -> ResultSet t l
buildResultSet' [] = ResultSet M.empty
buildResultSet' results =
  let getAId = fragmentAnswerId . fragmentMetaId . resultMetaData
      getFragId = fragmentId . fragmentMetaId . resultMetaData
      groupedByAnswerId = groupBy (\a -> \b -> getAId a == getAId b) results
      groupedByFragId =
        groupBy (\a -> \b -> (getFragId a) == (getFragId b)) <$> groupedByAnswerId
  in ResultSet $ foldl1 (M.unionWith (M.unionWith (++))) $ do
    aIdGroup <- groupedByFragId
    let aId = getAId (head $ head aIdGroup)
    fragIdGroup <- aIdGroup
    let fragId = getFragId $ head fragIdGroup
    return $ M.singleton aId (M.singleton fragId [fragIdGroup])


removeSubsumptionInSet:: Eq t => ResultSet t l -> ResultSet t l
removeSubsumptionInSet ResultSet{..}  =
  ResultSet $ fmap (
    fmap (\rs -> [removeSubsumption $ concat rs])
    ) resultSetMap

-- TODO: Update comment
-- | For each fragment remove all search results, that are properly subsumed by
-- another search result. Note that this will not remove all answer fragments
-- from a search result set, as there is always at least one search result per
-- answer fragment, that is not subsumed by another.
removeSubsumption :: (Eq t) => [SearchResult t l] -> [SearchResult t l]
removeSubsumption results = do
      r <- results
      if null $ filter (/= r) $ filter (subsumedByProper r) results
        then [r]
        else []


-- | Get the number of answers for which this result set contains alignment
-- match groups.
numberOfAnswers :: ResultSet t l -> Int
numberOfAnswers ResultSet{..} = M.size resultSetMap

-- |
numberOfFragments :: ResultSet t l -> Int
numberOfFragments ResultSet{..} = sum $ do
  (_, fragMap) <- M.toList resultSetMap
  return $ M.size fragMap

numberOfGroups :: ResultSet t l -> Int
numberOfGroups ResultSet{..} = sum $ do
  (_, fragMap) <- M.toList resultSetMap
  (_, groupList) <- M.toList fragMap
  return $ length groupList
