{-# LANGUAGE RecordWildCards #-}
module Thesis.Search.NGrams where

import Data.Foldable


-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence
ngrams :: Foldable f => Int -> f a -> [[a]]
ngrams n dat = do
  (ngr, _, _) <- ngramTails n dat
  return ngr


allNGrams :: Foldable f => Int -> f a -> [[a]]
allNGrams n dat = do
  (ngr, _, _) <- allNgramTails n dat
  return ngr

allNgramTails :: Foldable f => Int -> f a -> [([a], Int, [a])]
allNgramTails n dat | n <= 0 = []
                    | otherwise =
  let result = concat $ do
        x <- [0..n-1]
        let dat' = drop x (toList dat)
        [shift x $ ngrs dat']
  in length result `seq` result
  where
    ngrs xs = ngramTails n xs
    shift k xs = do
      (a,b,c) <- xs
      return (a, b+k, c)

-- | A list of n long nonoverlapping, contiguous subsequences of the given
-- sequence, paired with the remainder of the sequence starting with the
-- returned ngram.
--
-- So for example this for the string "abcdef" and 2-grams one of the values returned by this function would be ("cd", 2, "cdef")
ngramTails :: Foldable f => Int -> f a -> [([a], Int,  [a])]
ngramTails n dat = (ngr 0 $ toList dat)
  where
    ngr _ [] = []
    ngr k xs = let (ngram, rest) = splitAt n xs
               in if null rest && (length ngram < n)
                  then []
                  else (ngram, k, xs) : (ngr (k + n) rest)

