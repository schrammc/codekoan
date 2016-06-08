{-# LANGUAGE StandaloneDeriving, GADTs #-}
--------------------------------------------------------------------------------
-- |
--
-- This module defines a non compressed trie over any instance of 'Ord', where
-- the trie's leaves map to given values.
--
module Thesis.Trie where

import qualified Data.Map.Strict as M

import Data.Foldable


data Trie a v where
  TrieNode :: (Ord a) => M.Map a (Trie a v) -> Maybe v -> Trie a v

deriving instance (Show a, Show v) => Show (Trie a v)

deriving instance (Ord a, Eq v) => Eq (Trie a v)

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
mergeTriesWith f a@(TrieNode ma va) b@(TrieNode mb vb) =
  TrieNode (M.unionWith (mergeTriesWith f) ma mb) $! v
  where
    v = case (va, vb) of
      (Nothing, v') -> v'
      (Just v', Nothing) -> Just v'
      (Just v', Just v'') -> let val = f v' v''
                             in val `seq` (Just val)

-- | Discards the value from the right trie in case of conflicts
mergeTries :: (Eq v) => Trie a v -> Trie a v -> Trie a v
mergeTries = mergeTriesWith const

buildTrieWith :: (Foldable f, Foldable f', Ord a, Eq v)
                 => (v -> v -> v)
              -> f (f' a,v)
              -> Trie a v
buildTrieWith f xs = foldl' merge empty xs
  where
    merge t (xs, v) = mergeTriesWith f (linearTrie xs v) t

-- | Only uses the last value in the given sequence in case of conflict
buildTrie :: (Foldable f, Foldable f', Ord a, Eq v) => f (f' a,v) -> Trie a v
buildTrie = buildTrieWith const

-- | If the given sequence of things is in the trie, we return the value at it's
-- leaf
searchTrie :: (Foldable f) => Trie a v -> f a -> Maybe v
searchTrie tr = searchTrie' tr . toList

searchTrie' :: Trie a v -> [a] -> Maybe v
searchTrie' (TrieNode mp v) [] = v
searchTrie' (TrieNode mp v) (x:xs) = M.lookup x mp >>= \n -> searchTrie' n xs
