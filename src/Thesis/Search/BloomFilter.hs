{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Thesis.Search.BloomFilter where

import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy

import Data.Hashable

-- | A simple bloom filter implementation using the 'Hashable' type class.
-- Uses a phantom type in order to not mix up different types of content
data  BloomFilter a where
  BloomFilter :: (Hashable a) => BF.Bloom Int -> BloomFilter a

bloomMember :: Hashable a => BloomFilter a -> a -> Bool
bloomMember (BloomFilter f) x = (hash x) `BF.elem` f

(=?:) :: Hashable a => BloomFilter a -> a -> Bool
bl =?: x = bloomMember bl x

buildBloomList :: (Hashable a) => [a] -> BloomFilter a
buildBloomList xs = BloomFilter $ BF.Easy.easyList 0.01 (hash <$> xs)
