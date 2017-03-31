{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search.CompressedTrie where

import           Control.Applicative ((<|>))

import           Data.Foldable

import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))

import qualified Data.Vector as V
import           Data.Vector.Binary ()

import Control.DeepSeq

data CompressedTrie a v where
  CTrieNode :: (Ord a)
               => !(M.Map a (V.Vector a, CompressedTrie a v))
               -- Structural information of the trie
            -> !(Maybe v)
               -- The annotation of a completed word
            -> CompressedTrie a v
  CTrieLeaf :: !v -> CompressedTrie a v

deriving instance (Show a, Show v) => Show (CompressedTrie a v)

deriving instance (Ord a, Eq v) => Eq (CompressedTrie a v)

instance (NFData a, NFData v) => NFData (CompressedTrie a v) where
   rnf (CTrieLeaf v) = deepseq v ()
   rnf (CTrieNode mp v) = deepseq mp $ deepseq v ()

empty :: (Ord a) => CompressedTrie a v
empty = CTrieNode M.empty Nothing

linearTrie :: (Foldable f, Ord a) => f a -> v -> CompressedTrie a v
linearTrie !xs !v = linearTrie' (V.fromList $ toList xs) v

linearTrie' :: (Ord a) => V.Vector a -> v -> CompressedTrie a v
linearTrie' !vec !v | V.null vec = CTrieNode M.empty (Just v)
                    | otherwise  = CTrieNode mp Nothing
  where
    x = V.head vec
    mp = vec `seq` M.singleton x $! (vec, CTrieLeaf v)

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
    merge (va, ta) (vb, tb) | va == vb = (va, mergeTriesWith f ta tb)
                            | otherwise =
      let (common, ra, rb) = pref va vb
          nd | ra == V.empty =
            let new = CTrieNode (M.singleton (V.head rb) (rb, tb)) (g ta)
                other = snd $ fromJust $ M.lookup (V.head common) mp
            in mergeTriesWith f new other
             | rb == V.empty =
            let new = CTrieNode (M.singleton (V.head ra) (ra, ta)) (g tb)
                other = snd $ fromJust $ M.lookup (V.head common) mp'
            in mergeTriesWith f new other
             | otherwise = CTrieNode (M.fromList $ [(V.head ra, (ra,ta))
                                                   ,(V.head rb, (rb,tb))]) Nothing
      in nd `seq` (common, nd)
    pref va vb = let commons  = V.takeWhile (\(a,b) -> a == b) (V.zip va vb)
                     common   = fst $ V.unzip commons
                     n        = length common
                     (ra, rb) = (V.drop n va, V.drop n vb)
                 in common `seq` ra `seq` rb `seq` (common, ra, rb)
    g (CTrieLeaf x)  = Just x
    g (CTrieNode _ x) = x

-- | Build a suffix trie for a word. All leaves of the suffix trie will be
-- labelled with the given value.
--
-- Beware, that this implementation does not yet use a linear time suffix tree
-- construction algorithm like ukkonens and there is considerable room for
-- improvement in performance here!
buildSuffixTrie :: (Ord a, Eq v)
                   => Maybe Int -- ^ The minimum length of indexed suffixes
                -> V.Vector a -- ^ The word to index
                -> v   -- ^ An annotation, that will be used for all indexed
                       -- suffixes of all words
                -> CompressedTrie a v
buildSuffixTrie minSuffixLength xs v = buildTrie $ zip suffixes (repeat v)
  where
    n = fromMaybe 0 minSuffixLength
    suffixes = filter ((> n) . length) (vtails xs)

-- | Like Data.List.tails for Vector. This doesn't copy the vector's contents.
vtails :: V.Vector a -> [V.Vector a]
vtails v = do
  start <- [0 .. V.length v]
  let n = (V.length v) - start
  return $ V.slice start n v

buildTrieWith :: (Foldable f, Ord a, Eq v)
                 => (v -> v -> v)
              -> f (V.Vector a,v)
              -> CompressedTrie a v
buildTrieWith f xs = foldl' merge empty xs
  where
    merge t (ys, v) = mergeTriesWith f (linearTrie' ys v) t

-- | Only uses the last value in the given sequence in case of conflict
buildTrie :: (Foldable f, Ord a, Eq v) => f (V.Vector a,v)
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
        (_, (vec,t)) <- M.toList mp
        (xs, v) <- wordsInTrie' t
        [(V.toList vec <> xs, v)]
  in maybe childResults (\v' -> ([],v'):childResults) v

-- | Yield the values of this node and all it's children
trieLeaves :: CompressedTrie a v -> [v]
trieLeaves t = fst <$> trieLeavesDist t

-- | Yield the values of this node and it's children. With each value, this
-- function also returns the number of symbols on the path from this node to the
-- node containing a value.
trieLeavesDist :: CompressedTrie a v -> [(v, Int)]
trieLeavesDist (CTrieLeaf v) = [(v,0)]
trieLeavesDist (CTrieNode mp v) = ((, 0) <$> toList v) ++ do
  (_,(xs, nd)) <- M.toList mp
  let n = length xs
  (val, k) <- trieLeavesDist nd
  let count = k + n
  count `seq` return (val, count)

-- | A helper function. Applies the given function to both values if both are
-- defined. If only one of the values is defined, we return it. Nothing if no
-- value is defined
fOr :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
fOr f a b = (f <$> a <*> b) <|> a <|> b
