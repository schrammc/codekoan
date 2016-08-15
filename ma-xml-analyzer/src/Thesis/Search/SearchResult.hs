module Thesis.Search.SearchResult where

import Data.List (isSubsequenceOf)

import Thesis.CodeAnalysis.Language (LanguageText)
import Thesis.Data.Range
import Thesis.Data.Stackoverflow.Answer

data SearchResult t l =
  SearchResult { resultTextRange :: Range (LanguageText l)
                 -- ^ The matched text range in the query document
               , resultMatchedTokens :: [t]
               , resultMetaData :: AnswerFragmentMetaData
                 -- ^ Meta information about the matched answer fragment
               , resultFragmentRange :: Range t
                 -- ^ The range of matched tokens in the answer fragment
               , resultLevenScore :: Int
                 -- ^ Levenshtein distance of the search match
               }
  deriving (Eq, Show)

-- | Returns true if the second search result covers more than the first search
-- result and therefore makes the first search result redundant.
--
-- NOTE THAT THIS FUNCTION DOES NOT TAKE 'resultLevenScore' INTO CONSIDERATION.
subsumedBy :: (Eq t) => SearchResult t l -> SearchResult t l -> Bool
subsumedBy a b = fragmentSubsumption && textRangeSubsumption && tokenSubsumption
  where
    tokenSubsumption = isSubsequenceOf (resultMatchedTokens a)
                                       (resultMatchedTokens b)
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    textRangeSubsumption = isSubRangeOf (resultTextRange a)
                                        (resultTextRange b)

subsumedByProper :: (Eq t) => SearchResult t l -> SearchResult t l -> Bool
subsumedByProper a b =
  resultLevenScore a >= resultLevenScore b && subsumedBy a b
