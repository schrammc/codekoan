{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.CompressedTrie where

import Data.Binary

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Binary ()

import qualified Data.Map.Strict as M
import Data.Foldable

import Control.Applicative ((<|>))

data CompressedTrie a v where
  CTrieNode :: Ord a => !(M.Map a (Vector a, CompressedTrie a v))
            -> !(Maybe v)
            -> CompressedTrie a v
  CTrieLeaf :: !v -> CompressedTrie a v

deriving instance (Show a, Show v) => Show (CompressedTrie a v)

deriving instance (Ord a, Eq v) => Eq (CompressedTrie a v)

instance (Ord a, Binary a, Eq v, Binary v) => Binary (CompressedTrie a v) where
  put = put . wordsInTrie
  get = do
    wordsVector <- get :: Get (V.Vector (V.Vector a, v))
    return $! buildTrie wordsVector

empty :: (Ord a) => CompressedTrie a v
empty = CTrieNode M.empty Nothing

linearTrie :: (Foldable f, Ord a) => f a -> v -> CompressedTrie a v
linearTrie = linearTrie' . toList

-- | Helper function for 'linearTrie' that works with lists
linearTrie' :: (Ord a) => [a] -> v -> CompressedTrie a v
linearTrie' [] v = CTrieNode M.empty (Just v)
linearTrie' xs@(x:_) v = CTrieNode mp Nothing
  where
    leaf = CTrieLeaf v
    mp = leaf `seq` M.singleton x $! (V.fromList xs, leaf)


-- | Merge two tries with a merging function if two values are at the end of the
-- same path
mergeTriesWith :: (Eq v) => (v -> v -> v)
               -> CompressedTrie a v
               -> CompressedTrie a v
               -> CompressedTrie a v
mergeTriesWith f (CTrieLeaf v   ) (CTrieLeaf v'   )  = CTrieLeaf $ f v v'
mergeTriesWith f l@(CTrieLeaf _ ) n@(CTrieNode _ _)  = mergeTriesWith f n l
mergeTriesWith f (CTrieNode mp v) (CTrieLeaf v'   )  =
  CTrieNode mp $ fOr f v (Just v')
mergeTriesWith f (CTrieNode mp v) (CTrieNode mp' v') =
  CTrieNode (M.unionWith merge mp mp') (fOr f v v')
  where
    merge (va, ta) (vb, tb) | va == vb = (va, mergeTriesWith f ta tb)
                            | otherwise =
      let (common, ra, rb) = pref va vb
          nd | ra == V.empty = CTrieNode (M.singleton (V.head rb) (rb, tb))(g ta)
             | rb == V.empty = CTrieNode (M.singleton (V.head ra) (ra, ta))(g tb)
             | otherwise = CTrieNode (M.fromList $ [(V.head ra, (ra,ta))
                                                   ,(V.head rb, (rb,tb))]) Nothing
      in (common, nd)
    pref va vb = let (commons, rest) = V.span (\(a,b) -> a == b) (V.zip va vb)
                     common = fst $ V.unzip commons
                     (ra, rb) = V.unzip rest
                 in (common, ra, rb)
    g (CTrieLeaf x)  = Just x
    g (CTrieNode _ x) = x

-- | Discards the value from the right trie in case of conflicts
mergeTries :: (Eq v) => CompressedTrie a v
           -> CompressedTrie a v
           -> CompressedTrie a v
mergeTries = mergeTriesWith const

buildTrieWith :: (Foldable f, Foldable f', Ord a, Eq v)
                 => (v -> v -> v)
              -> f (f' a,v)
              -> CompressedTrie a v
buildTrieWith f xs = foldl' merge empty xs
  where
    merge t (ys, v) = mergeTriesWith f (linearTrie ys v) t

-- | Only uses the last value in the given sequence in case of conflict
buildTrie :: (Foldable f, Foldable f', Ord a, Eq v) => f (f' a,v)
          -> CompressedTrie a v
buildTrie = buildTrieWith const

wordsInTrie :: CompressedTrie a v -> V.Vector (V.Vector a, v)
wordsInTrie t = V.fromList $ do
  (w,v) <- wordsInTrie' t
  [(V.fromList w, v)]

wordsInTrie' :: CompressedTrie a v -> [([a],v)]
wordsInTrie' (CTrieLeaf v) = [([],v)]
wordsInTrie' (CTrieNode mp v) =
  let childResults = do
        (_, (vec,t)) <- M.toList mp
        (xs, v) <- wordsInTrie' t
        [(V.toList vec ++ xs, v)]
  in maybe childResults (\v' -> ([],v'):childResults) v

-- | A helper function. Applies the given function to both values if both are
-- defined. If only one of the values is defined, we return it. Nothing if no
-- value is defined
fOr :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
fOr f a b = (f <$> a <*> b) <|> a <|> b
