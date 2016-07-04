{-# LANGUAGE RecordWildCards #-}
module Thesis.Search.ResultSet where

import           Data.List (groupBy)
import qualified Data.Map.Strict as M

import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.SearchResult

-- | Search results organized into questions and fragments of these questions
newtype ResultSet t =
  ResultSet {resultSetMap :: (M.Map AnswerId (M.Map Int [SearchResult t]))}

listOfResults :: ResultSet t -> [SearchResult t]
listOfResults (ResultSet mp) = do
  (_, mp') <- M.toList mp
  (_, res) <- M.toList mp'
  res

-- | Filter out fragments from a result set, that are below a certain fraction
-- of coverage. If no fragment for an answer has sufficient coverage, we
-- filter out the answer.
answersWithCoverage :: (Eq t)
                        => Double -- ^ A fraction (>= 0 and <= 1)
                        -> ResultSet t
                        -> ResultSet t
answersWithCoverage cov ResultSet{..} = ResultSet $ 
  (flip M.mapMaybeWithKey) resultSetMap $ \_ -> \mp ->
    let mp' = (flip M.mapMaybeWithKey) mp $ \_ -> \results ->
               case results of
                 [] -> Nothing
                 rs@(r:_) ->
                     let n = fragmentMetaSize $ resultMetaData r
                         frags = resultFragmentRange <$> rs
                     in if coveragePercentage n frags >= cov'
                        then Just rs
                        else Nothing
    in if M.empty == mp'
       then Nothing
       else Just mp'
  where
    cov' = max 0 (min 1 cov)

-- | Build a result set from a list of search results.
buildResultSet :: [SearchResult t] -> ResultSet t
buildResultSet [] = ResultSet M.empty
buildResultSet results =
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
    return $ M.singleton aId (M.singleton fragId fragIdGroup)
