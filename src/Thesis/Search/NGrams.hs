{-# LANGUAGE RecordWildCards #-}
module Thesis.Search.NGrams where

import Data.Foldable


-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence
ngrams :: Foldable f => Int -> f a -> [[a]]
ngrams n dat = fst <$> ngramTails n dat


allNGrams :: Foldable f => Int -> f a -> [[a]]
allNGrams n dat = fst <$> allNgramTails n dat

allNgramTails :: Foldable f => Int -> f a -> [([a], [a])]
allNgramTails n dat | n <= 0 = []
                    | otherwise =
  let result = concat $ do
        x <- [0..n-1]
        let dat' = drop x (toList dat)
        [ngrs dat']
  in length result `seq` result
  where
    ngrs xs = ngramTails n xs

-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence, paired with the remainder of the sequence starting with the
-- returned ngram.
--
-- So for example this for the string "abcdef" and 2-grams one of the values returned by this function would be ("cd","cdef")
ngramTails :: Foldable f => Int -> f a -> [([a], [a])]
ngramTails n dat = (ngr $ toList dat)
  where
    ngr [] = []
    ngr xs = let (ngram, rest) = splitAt n xs
             in if null rest && (length ngram < n)
                then []
                else (ngram, xs) : (ngr rest)

