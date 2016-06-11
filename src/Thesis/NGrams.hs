{-# LANGUAGE RecordWildCards #-}
module Thesis.NGrams where

import Data.Foldable
import Data.Hashable

import Thesis.BloomFilter

-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence
ngrams :: Foldable f => Int -> f a -> [[a]]
ngrams n dat = fst <$> ngramTails n dat

-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence, paired with the remainder of the sequence starting with the
-- returned ngram.
--
-- So for example this for the string "abcdef" and 2-grams one of the values returned by this function would be ("cd","cdef")
ngramTails :: Foldable f => Int -> f a -> [([a], [a])]
ngramTails n dat = ngr $ toList dat
  where
    ngr [] = []
    ngr xs = let (ngram, rest) = splitAt n xs
             in if null rest && (length ngram < n)
                then []
                else (ngram, xs) : (ngr rest)

-- | Build a bloomfilter containing hashes of all ngrams in the given strings
ngramBloom :: (Foldable f, Hashable a) => Int -> f a -> BloomFilter [a]
ngramBloom n = buildBloom . (ngrams n)
