-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- A graph data structure along with some basic -- fairly performantly
-- implemented -- algorithms.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Thesis.Data.Graph
       ( -- * Graphs
         Graph

         -- * Construction
       , buildGraphUnsafe

         -- * Algorithms
       , connected
       , cliques
       ) where

import           Data.Algorithm.MaximalCliques

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.List
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

-- | Build a bidirectional graph from a vector of vertices and a list of
-- edges that are given as pairs of indices. Edges to nodes not in the graph
-- will be rejected.
--
-- /This assumes the edges list is sorted by 'fst'/
buildGraphUnsafe :: V.Vector a -> [(Int, Int)] -> Graph a
buildGraphUnsafe graphNodes edges = Graph{..}
  where
    -- safe variant woult be fromListWith (IS.union) in graphEdges = (...) $ do
    --
    -- this is a bit faster though.
    graphEdges = IM.fromListWith (IS.union) $ do
      edgeGroup <- groupBy (\(a, _) -> \(b, _) -> a == b) preparedEdges
      let edgeSource = fst $ head edgeGroup
          edgeTargets = IS.fromList $ snd <$> edgeGroup
      return (edgeSource, edgeTargets)
    
    preparedEdges = filter (\(a,b) -> a /= b
                                      && a < V.length graphNodes
                                      && b < V.length graphNodes
                                      && a >= 0 && b >= 0
                           ) edges

-- | A graph with directed edges.
data Graph a = Graph { graphNodes :: !(V.Vector a)
                     , graphEdges :: !(IntMap IntSet)
                     }
             deriving (Show)

-- | Answers the questions if two nodes are connected. Nodes can't be connected
-- to themselves (i.e. 'connected gr a a == False')
connected :: Graph a -> Int -> Int -> Bool
connected Graph{..} !a !b | a == b = False
                          | otherwise = fromMaybe False $ do
                            neighbours <- IM.lookup (min a b) graphEdges
                            return $ IS.member (max a b) neighbours

neighbourIndices :: Graph a -> Int -> IS.IntSet
neighbourIndices Graph{..} n = IS.union outNeighbours inNeighbours
  where
    outNeighbours = (IM.!) graphEdges n
    inNeighbours = IS.fromAscList $ do
      (k, outNs) <- IM.toAscList graphEdges
      if IS.member n outNs
         then return k
         else []

-- | Find all distinct maxial cliques in a graph. A maximal clique is any clique
-- that can't be enlarged by addition of a node.
cliques :: Graph a -> [[a]]
cliques gr@Graph{..} = do
  cs <- cliques' gr
  return $ V.unsafeIndex graphNodes <$> cs 

-- | Find all distinct maxial cliques in a graph. A maximal clique is any clique
-- that can't be enlarged by addition of a node. This returns the indices of the
-- resulting nodes.
cliques' :: Graph a -> [[Int]]
cliques' gr@(Graph{..}) =
  IS.toList <$> maximalCliques pickpivot f (IM.keysSet graphEdges)
  where
    f = neighbourIndices gr
    pickpivot p x = head $ IS.elems p ++ IS.elems x
{-# INLINE cliques' #-}
