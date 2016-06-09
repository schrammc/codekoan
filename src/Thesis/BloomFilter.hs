{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.BloomFilter ( BloomFilter
                            -- * Elementary functions
                          , emptyBloom
                          , bloomSingleton
                            -- * Building Bloom filters
                          , buildBloom
                          , bloomMerge
                            -- * Querying
                          , bloomMember
                          , (=?:)
                          ) where

import Data.Bits
import Data.Binary (Binary)

import GHC.Generics (Generic)

import Data.Hashable

-- | A simple bloom filter implementation using the 'Hashable' type class.
-- Uses a phantom type in order to not mix up different types of content
newtype BloomFilter a = BloomFilter { bloomToInt :: Int }
                      deriving (Eq, Generic)

instance Binary (BloomFilter a)

-- | A bloom filter containing no element
emptyBloom :: (Hashable a) => BloomFilter a
emptyBloom = BloomFilter 0

-- | A bloom filter containing a single element
bloomSingleton :: (Hashable a) => a -> BloomFilter a
bloomSingleton = buildBloom . (\x -> [x])

-- | Build a bloom filter from a list of hashable items
buildBloom :: Hashable a => [a] -> BloomFilter a
buildBloom = BloomFilter . (foldl (\bloom -> \x -> bloom .|. (hash x)) 0)

-- | /O(1)/ Membership detection in the bloom filter
bloomMember :: Hashable a => BloomFilter a -> a -> Bool
bloomMember (BloomFilter{..}) v = (hash v) == bloomToInt .&. (hash v)

-- | /O(1)/ Membership detection in the bloom filter.
-- Synonym of 'bloomMember'.
(=?:) :: Hashable a => BloomFilter a -> a -> Bool
bl =?: x = bloomMember bl x

-- | /O(1)/ Merge two bloom filters.
bloomMerge :: BloomFilter a -> BloomFilter a -> BloomFilter a
bloomMerge f f' = BloomFilter (bloomToInt f .|. bloomToInt f')
