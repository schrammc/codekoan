-- |
-- Description: Compressed tries with vectors as edge labels
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
-- Compressed tries with vectors as edge labels
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
module Thesis.Search.CompressedTrie where

import           Control.Applicative ((<|>))

import           Data.Foldable

import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           Data.Vector.Binary ()
import           Data.Hashable
import           Control.DeepSeq

data CompressedTrie a v where
  CTrieNode :: (Eq a, Hashable a)
               => !(M.HashMap a (V.Vector a, Int, CompressedTrie a v))
               -- ^ Structural information of the trie
               --   * Label
               --   * LabelLength (Performance reasons)
               --   * Next node
            -> !(Maybe v)
               -- ^ The annotation of a completed word
            -> CompressedTrie a v
  CTrieLeaf :: !v -> CompressedTrie a v

deriving instance (Show a, Show v) => Show (CompressedTrie a v)

deriving instance (Hashable a, Eq a, Eq v) => Eq (CompressedTrie a v)

instance (NFData a, NFData v) => NFData (CompressedTrie a v) where
   rnf (CTrieLeaf v) = deepseq v ()
   rnf (CTrieNode mp v) = deepseq mp $ deepseq v ()

nodeValue :: CompressedTrie a v -> Maybe v
nodeValue (CTrieNode _ maybeV) = maybeV
nodeValue (CTrieLeaf v)        = Just v
{-# INLINE nodeValue #-}

empty :: (Eq a, Hashable a) => CompressedTrie a v
empty = CTrieNode M.empty Nothing

linearTrie :: (Foldable f, Eq a, Hashable a) => f a -> v -> CompressedTrie a v
linearTrie !xs !v = linearTrie' (V.fromList $ toList xs) v

linearTrie' :: (Eq a, Hashable a) => V.Vector a -> v -> CompressedTrie a v
linearTrie' !vec !v | vectorLength == 0 = CTrieNode M.empty (Just v)
                    | otherwise  = CTrieNode mp Nothing
  where
    !vectorLength = V.length vec
    x = V.head vec
    mp = vec `seq` M.singleton x $! (vec, vectorLength, CTrieLeaf v)

-- | Merge two tries with a merging function if two values are at the end of the
-- same path
mergeTriesWith :: (Eq v) => (v -> v -> v)
               -> CompressedTrie a v
               -> CompressedTrie a v
               -> CompressedTrie a v
mergeTriesWith f !(CTrieLeaf v   ) !(CTrieLeaf v'   )  = CTrieLeaf $ f v v'
mergeTriesWith f !l@(CTrieLeaf _ ) !n@(CTrieNode _ _)  = mergeTriesWith f n l
mergeTriesWith f !(CTrieNode mp v) !(CTrieLeaf v'   )  =
  CTrieNode mp $ fOr f v (Just v')
mergeTriesWith f !(CTrieNode mp v) !(CTrieNode mp' v') =
  CTrieNode (M.unionWith merge mp mp') (fOr f v v')
  where
    thrd (_, _, x) = x
    merge (va, la, ta) (vb, lb, tb) | V.length va == V.length vb &&
                                      va == vb = (va, la, mergeTriesWith f ta tb)
                                    | otherwise =
      let (common, ra, rb) = pref va vb
          nd | V.null ra =
            let new =
                  CTrieNode (M.singleton (V.head rb) (rb, V.length rb, tb)) (g ta)
                other = thrd $ fromJust $ M.lookup (V.head common) mp
            in mergeTriesWith f new other
             | V.null rb =
            let new =
                  CTrieNode (M.singleton (V.head ra) (ra, V.length ra, ta)) (g tb)
                other = thrd $ fromJust $ M.lookup (V.head common) mp'
            in mergeTriesWith f new other
             | otherwise =
                    CTrieNode (M.fromList $ [(V.head ra, (ra, V.length ra, ta))
                                            ,(V.head rb, (rb, V.length rb, tb))])
                              Nothing
      in nd `seq` (common, V.length common, nd)
    pref va vb = let !common = largestCommonPrefix va vb
                     !n     = V.length common
                     !ra    = V.drop n va
                     !rb    = V.drop n vb
                 in (common, ra, rb)
    g (CTrieLeaf x)  = Just x
    g (CTrieNode _ x) = x

largestCommonPrefix :: (Eq a) => V.Vector a -> V.Vector a  -> V.Vector a
largestCommonPrefix !va !vb | minLength == 0 = V.empty
                            | otherwise = V.unsafeSlice 0 (go 0) va
  where
    !minLength = min (V.length va) (V.length vb)
    go !i | i == minLength = i
          | V.unsafeIndex va i == V.unsafeIndex vb i = go (i+1)
          | otherwise = i

-- | Build a suffix trie for a word. All leaves of the suffix trie will be
-- labelled with the given value.
--
-- Beware, that this implementation does not yet use a linear time suffix tree
-- construction algorithm like ukkonens and there is considerable room for
-- improvement in performance here!
buildSuffixTrie :: (Hashable a, Eq a, Eq v)
                   => Maybe Int -- ^ The minimum length of indexed suffixes
                -> V.Vector a -- ^ The word to index
                -> v   -- ^ An annotation, that will be used for all indexed
                       -- suffixes of all words
                -> CompressedTrie a v
buildSuffixTrie minSuffixLength xs v = buildTrie $ zip suffixes (repeat v)
  where
    n = fromMaybe 0 minSuffixLength
    suffixes = filter ((>= n) . V.length) (vtails xs)


buildLengthAnnotatedSuffixTrie :: (Hashable a, Eq a) =>
                                  Maybe Int
                               -> V.Vector a
                               -> CompressedTrie a Int
buildLengthAnnotatedSuffixTrie minSuffixLength xs =
  buildTrie $ zip suffixes [0..]
  where 
    n = fromMaybe 0 minSuffixLength
    suffixes = filter ((>= n) . V.length) (vtails xs)

-- | Like Data.List.tails for Vector. This doesn't copy the vector's contents.
vtails :: V.Vector a -> [V.Vector a]
vtails v = do
  start <- [0 .. len]
  return $ V.unsafeSlice start (len - start) v
  where
    !len = V.length v

vinits :: V.Vector a -> [V.Vector a]
vinits !v | V.null v = []
          | otherwise = v:(vinits $! V.init v)

buildTrieWith :: (Foldable f, Hashable a, Eq a, Eq v)
                 => (v -> v -> v)
              -> f (V.Vector a,v)
              -> CompressedTrie a v
buildTrieWith f xs = foldl' merge empty xs
  where
    merge t (ys, v) = mergeTriesWith f (linearTrie' ys v) t

-- | Only uses the last value in the given sequence in case of conflict
buildTrie :: (Foldable f, Hashable a, Eq a, Eq v) => f (V.Vector a,v)
          -> CompressedTrie a v
buildTrie = buildTrieWith const

wordsInTrie :: CompressedTrie a v -> V.Vector (V.Vector a, v)
wordsInTrie t = V.fromList $ do
  (w,v) <- wordsInTrie' t
  [(V.fromList w, v)]

-- | A list of words in the trie complete with their annotation
wordsInTrie' :: CompressedTrie a v -> [([a],v)]
wordsInTrie' (CTrieLeaf v) = [([],v)]
wordsInTrie' (CTrieNode mp v) =
  let childResults = do
        (_, (vec, _,t)) <- M.toList mp
        (xs, v) <- wordsInTrie' t
        [(V.toList vec <> xs, v)]
  in maybe childResults (\v' -> ([],v'):childResults) v

-- | Yield the values of this node and all it's children
trieLeaves :: CompressedTrie a v -> Seq v
trieLeaves t = fst <$> trieLeavesDist t

trieLeavesDist :: CompressedTrie a v -> Seq (v, Int)
trieLeavesDist = trieLeavesDist' 0

-- | Yield the values of this node and it's children. With each value, this
-- function also returns the number of symbols on the path from this node to the
-- node containing a value.
trieLeavesDist' :: Int -> CompressedTrie a v -> Seq (v, Int)
trieLeavesDist' !n (CTrieLeaf v) = Seq.singleton (v,n)
trieLeavesDist' !n (CTrieNode mp v) = foldl' f current mp
  where
    f xs (_, labelLength, nd) = xs >< (trieLeavesDist' (n + labelLength) nd)
    current = case v of
                Nothing -> Seq.empty
                Just x  -> Seq.singleton (x,n)

-- | A helper function. Applies the given function to both values if both are
-- defined. If only one of the values is defined, we return it. Nothing if no
-- value is defined
fOr :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
fOr f a b = (f <$> a <*> b) <|> a <|> b
