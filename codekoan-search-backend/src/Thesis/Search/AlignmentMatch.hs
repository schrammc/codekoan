-- |
-- Description: Code similarity in alignment matches
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
-- Alignment matches are the basic "unit of similarity" upon which large parts
-- of the search pipeline operate.
--
{-# LANGUAGE BangPatterns #-}
module Thesis.Search.AlignmentMatch where

import Thesis.CodeAnalysis.Language (LanguageText)
import Thesis.Data.Range
import Control.DeepSeq

data AlignmentMatch t l ann =
  AlignmentMatch { resultTextRange :: (Range (LanguageText l))
                   -- ^ The matched text range in the query document
                 , resultQueryRange :: {-# UNPACK #-} !(Range t)
                 , resultMetaData :: {-# UNPACK #-} !ann
                   -- ^ Meta information about the matched answer fragment
                 , resultFragmentRange :: {-# UNPACK #-} !(Range t)
                   -- ^ The range of matched tokens in the answer fragment
                 , resultLevenScore :: {-# UNPACK #-} !Int
                   -- ^ Levenshtein distance of the search match
                 }
  deriving (Eq, Show)

instance (NFData t, NFData ann) => NFData (AlignmentMatch t l ann) where
  rnf am = resultTextRange am `deepseq`
           resultQueryRange am `deepseq`
           resultMetaData am `deepseq`
           resultFragmentRange am `deepseq`
           resultLevenScore am `deepseq` ()

-- | Same as 'subsumedBy' assuming a and b are matched to the same resultMeta ann
subsumedProperSame !a !b = queryRangeSubsumption && fragmentSubsumption && proper
  where
    proper = resultLevenScore a >= resultLevenScore b
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    queryRangeSubsumption = isSubRangeOf (resultQueryRange a)
                                         (resultQueryRange b)

-- | Returns true if the second search result covers more than the first search
-- result and therefore makes the first search result redundant.
--
-- NOTE THAT THIS FUNCTION DOES NOT TAKE 'resultLevenScore' INTO CONSIDERATION.
subsumedBy :: (Eq t, Eq ann) =>
              AlignmentMatch t l ann
           -> AlignmentMatch t l ann
           -> Bool
subsumedBy !a !b = sameFragment &&
                 fragmentSubsumption &&
                 textRangeSubsumption -- &&
--                 tokenSubsumption
  where
    sameFragment = resultMetaData a == resultMetaData b
--    tokenSubsumption = isSubsequenceOf (resultMatchedTokens a)
--                                       (resultMatchedTokens b)
    fragmentSubsumption = isSubRangeOf (resultFragmentRange a)
                                       (resultFragmentRange b)
    textRangeSubsumption = isSubRangeOf (resultQueryRange a)
                                        (resultQueryRange b)

subsumedByProper :: (Eq t, Eq ann) =>
                    AlignmentMatch t l ann
                 -> AlignmentMatch t l ann
                 -> Bool
subsumedByProper !a !b =
  resultLevenScore a >= resultLevenScore b && subsumedBy a b
