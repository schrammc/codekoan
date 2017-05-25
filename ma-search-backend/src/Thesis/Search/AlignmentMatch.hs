module Thesis.Search.AlignmentMatch where

import Data.List (isSubsequenceOf)

import Thesis.CodeAnalysis.Language (LanguageText)
import Thesis.Data.Range
import Control.DeepSeq

data AlignmentMatch t l ann =
  AlignmentMatch { resultTextRange :: !(Range (LanguageText l))
                   -- ^ The matched text range in the query document
                 , resultMatchedTokens :: [t]
                 , resultQueryRange :: !(Range t)
                 , resultMetaData :: !ann
                   -- ^ Meta information about the matched answer fragment
                 , resultFragmentRange :: !(Range t)
                   -- ^ The range of matched tokens in the answer fragment
                 , resultLevenScore :: !Int
                   -- ^ Levenshtein distance of the search match
                 }
  deriving (Eq, Show)

instance (NFData t, NFData ann) => NFData (AlignmentMatch t l ann) where
  rnf am = resultTextRange am `deepseq`
           resultMatchedTokens am `deepseq`
           resultQueryRange am `deepseq`
           resultMetaData am `deepseq`
           resultFragmentRange am `deepseq`
           resultLevenScore am `deepseq` ()

-- | Same as 'subsumedBy' assuming a and b are matched to the same resultMeta ann
subsumedProperSame a b = textRangeSubsumption && fragmentSubsumption && proper
  where
    proper = resultLevenScore a >= resultLevenScore b
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    textRangeSubsumption = isSubRangeOf (resultTextRange a)
                                        (resultTextRange b)

-- | Returns true if the second search result covers more than the first search
-- result and therefore makes the first search result redundant.
--
-- NOTE THAT THIS FUNCTION DOES NOT TAKE 'resultLevenScore' INTO CONSIDERATION.
subsumedBy :: (Eq t, Eq ann) =>
              AlignmentMatch t l ann
           -> AlignmentMatch t l ann
           -> Bool
subsumedBy a b = sameFragment &&
                 fragmentSubsumption &&
                 textRangeSubsumption -- &&
--                 tokenSubsumption
  where
    sameFragment = resultMetaData a == resultMetaData b
--    tokenSubsumption = isSubsequenceOf (resultMatchedTokens a)
--                                       (resultMatchedTokens b)
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    textRangeSubsumption = isSubRangeOf (resultTextRange a)
                                        (resultTextRange b)

subsumedByProper :: (Eq t, Eq ann) =>
                    AlignmentMatch t l ann
                 -> AlignmentMatch t l ann
                 -> Bool
subsumedByProper a b =
  resultLevenScore a >= resultLevenScore b && subsumedBy a b
