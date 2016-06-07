{-# LANGUAGE StandaloneDeriving, GADTs #-}
--------------------------------------------------------------------------------
-- |
--
-- This module defines a non compressed trie over any instance of 'Ord', where
-- the trie's leaves map to given values.
--
module Thesis.Trie where

import qualified Data.Map as M

import Data.Foldable


data Trie a v where
  LeafNode :: (Ord a) => v -> Trie a v
  InnerNode :: (Ord a) => M.Map a (Trie a v) -> Trie a v

deriving instance (Show a, Show v) => Show (Trie a v)

deriving instance (Ord a, Eq v) => Eq (Trie a v)

empty :: (Ord a) => Trie a v
empty = InnerNode M.empty

linearTrie :: (Foldable f, Ord a) => f a -> v -> Trie a v
linearTrie = linearTrie' . toList

-- | Helper function for 'linearTrie' that works with lists
linearTrie' :: (Ord a) => [a] -> v -> Trie a v
linearTrie' [] v = LeafNode v
linearTrie' (x:xs) v = InnerNode $ M.singleton x (linearTrie' xs v)

-- | Merge two tries with a merging function if two values are at the end of the
-- same path
mergeTriesWith :: (Eq v) => (v -> v -> v) -> Trie a v -> Trie a v -> Trie a v
mergeTriesWith f (LeafNode a) (LeafNode b) = LeafNode $ f a b
mergeTriesWith f (LeafNode _) (InnerNode _) = error "Trie.merge"
mergeTriesWith f (InnerNode _) (LeafNode _) = error "Trie.merge"
mergeTriesWith f a@(InnerNode ma) b@(InnerNode mb) =
  InnerNode $ M.unionWith (mergeTriesWith f) ma mb

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
searchTrie (LeafNode v) xs | null xs = Just v
searchTrie n xs = case foldlM f n xs of
  Just (LeafNode v) -> Just v
  _ -> Nothing
  where
    f (InnerNode mp) x = M.lookup x mp
    f _ _ = Nothing

