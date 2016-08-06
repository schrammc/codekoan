module Thesis.Search.SearchResult where

import Data.List (isSubsequenceOf)

import Thesis.Data.Range
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Text.PositionRange

data SearchResult t  =
  SearchResult { resultTextRange :: Range
               , resultMatchedTokens :: [t]
               , resultMetaData :: AnswerFragmentMetaData
               , resultFragmentRange :: Range
               , resultLevenScore :: Int
               }
  deriving (Eq, Show)

-- | Returns true if the second search result covers more than the first search
-- result and therefore makes the first search result redundant.
--
-- NOTE THAT THIS FUNCTION DOES NOT TAKE 'resultLevenScore' INTO CONSIDERATION.
subsumedBy :: (Eq t) => SearchResult t -> SearchResult t -> Bool
subsumedBy a b = fragmentSubsumption && textRangeSubsumption && tokenSubsumption
  where
    tokenSubsumption = isSubsequenceOf (resultMatchedTokens a)
                                       (resultMatchedTokens b)
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    textRangeSubsumption = isSubRangeOf (resultTextRange a)
                                        (resultTextRange b)

subsumedByProper :: (Eq t) => SearchResult t -> SearchResult t -> Bool
subsumedByProper a b =
  resultLevenScore a >= resultLevenScore b && subsumedBy a b
