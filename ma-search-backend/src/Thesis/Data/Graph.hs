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
    graphEdges = IM.fromList $ do
      edgeGroup <- groupBy (\(a, _) -> \(b, _) -> a == b) preparedEdges
      let edgeSource = fst $ head edgeGroup
          edgeTargets = IS.fromList $ snd <$> edgeGroup
      return (edgeSource, edgeTargets)
    
    preparedEdges = formatEdge <$> filter (\(a,b) -> a /= b
                                                     && a < V.length graphNodes
                                                     && b < V.length graphNodes
                                                     && a >= 0 && b >= 0
                                          ) edges
    formatEdge (a,b) = (min a b, max a b)

-- | A graph with undirected edges.
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
cliques' Graph{..} =
  IS.toList <$> maximalCliques pickpivot f (IM.keysSet graphEdges)
  where
    f = (IM.!) graphEdges
    pickpivot p x = head $ IS.elems p ++ IS.elems x
{-# INLINE cliques' #-}
