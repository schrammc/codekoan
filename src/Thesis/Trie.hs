{-# LANGUAGE StandaloneDeriving, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
--
-- This module defines a non compressed trie over any instance of 'Ord', where
-- the trie's leaves map to given values.
--
module Thesis.Trie where

import Data.Binary

import qualified Data.Map.Strict as M

import Data.Foldable
import qualified Data.Vector as V
import Data.Vector.Binary ()

data Trie a v where
  TrieNode :: (Ord a) => !(M.Map a (Trie a v)) -> !(Maybe v) -> Trie a v

deriving instance (Show a, Show v) => Show (Trie a v)

deriving instance (Ord a, Eq v) => Eq (Trie a v)

instance (Ord a, Binary a, Eq v, Binary v) => Binary (Trie a v) where
  put = put . wordsInTrie
  get = do
    wordsVector <- get :: Get (V.Vector (V.Vector a, v))
    return $! buildTrie wordsVector

empty :: (Ord a) => Trie a v
empty = TrieNode M.empty Nothing

linearTrie :: (Foldable f, Ord a) => f a -> v -> Trie a v
linearTrie = linearTrie' . toList

-- | Helper function for 'linearTrie' that works with lists
linearTrie' :: (Ord a) => [a] -> v -> Trie a v
linearTrie' [] v = TrieNode M.empty (Just v)
linearTrie' (x:xs) v = TrieNode (M.singleton x (linearTrie' xs v)) Nothing

-- | Merge two tries with a merging function if two values are at the end of the
-- same path
mergeTriesWith :: (Eq v) => (v -> v -> v) -> Trie a v -> Trie a v -> Trie a v
mergeTriesWith f (TrieNode ma va) (TrieNode mb vb) =
  TrieNode (M.unionWith (mergeTriesWith f) ma mb) $! v
  where
    v = case (va, vb) of
      (Nothing, v') -> v'
      (Just v', Nothing) -> Just v'
      (Just v', Just v'') -> Just $! f v' v''

-- | Discards the value from the right trie in case of conflicts
mergeTries :: (Eq v) => Trie a v -> Trie a v -> Trie a v
mergeTries = mergeTriesWith const

buildTrieWith :: (Foldable f, Foldable f', Ord a, Eq v)
                 => (v -> v -> v)
              -> f (f' a,v)
              -> Trie a v
buildTrieWith f xs = foldl' merge empty xs
  where
    merge t (ys, v) = mergeTriesWith f (linearTrie ys v) t

-- | Only uses the last value in the given sequence in case of conflict
buildTrie :: (Foldable f, Foldable f', Ord a, Eq v) => f (f' a,v) -> Trie a v
buildTrie = buildTrieWith const

-- | If the given sequence of things is in the trie, we return the value at it's
-- leaf
searchTrie :: (Foldable f) => Trie a v -> f a -> Maybe v
searchTrie tr = searchTrie' tr . toList

searchTrie' :: Trie a v -> [a] -> Maybe v
searchTrie' (TrieNode _ v) [] = v
searchTrie' (TrieNode mp _) (x:xs) = M.lookup x mp >>= \n -> searchTrie' n xs

-- | Count the number of entries in a node
countEntries :: (v -> Int) -> Trie a v -> Int
countEntries f (TrieNode mp v) = n + (sum $ countEntries f <$> mp)
  where
    n = maybe 0 f v

-- | Builds a vector of words with their associated values from a trie.
-- The following holds @buildTrie (wordsInTrie tr) == tr@
wordsInTrie :: Trie a v -> V.Vector (V.Vector a, v)
wordsInTrie t = V.fromList $ do
  (w,v) <- wordsInTrie' t
  [(V.fromList w, v)]

wordsInTrie' :: Trie a v -> [([a], v)]
wordsInTrie' (TrieNode mp v) =
  let childResults = do
        (x,child) <- M.toList mp
        (xs, v') <- wordsInTrie' child
        return (x:xs, v')
   in maybe childResults (\v' -> ([],v'):childResults) v
