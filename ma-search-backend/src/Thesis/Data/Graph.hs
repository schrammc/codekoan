-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- A graph data structure along with some basic -- fairly performantly
-- implemented -- algorithms.

{-# LANGUAGE RecordWildCards #-}
module Thesis.Data.Graph
       ( -- * Graphs
         Graph

         -- * Construction
       , buildGraph

         -- * Algorithms
       , connected
       , cliques
       ) where

import Data.Algorithm.MaximalCliques

import qualified Data.Vector as V
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- | Safely build a bidirectional graph from a vector of vertices and a list of
-- edges that are given as pairs of indices. Edges to nodes not in the graph
-- will be rejected.
buildGraph :: V.Vector a -> [(Int, Int)] -> Graph a
buildGraph graphNodes edges = Graph{..}
  where
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
data Graph a = Graph { graphNodes :: V.Vector a
                     , graphEdges :: IntMap IntSet
                     }
               deriving (Show)

-- | Answers the questions if two nodes are connected. Nodes can't be connected
-- to themselves (i.e. 'connected gr a a == False')
connected :: Graph a -> Int -> Int -> Bool
connected Graph{..} a b | a == b = False
                        | otherwise = maybe False id $ do
                          neighbours <- IM.lookup x graphEdges
                          return $ IS.member y neighbours
  where
    x = min a b
    y = max a b

-- | Find all distinct maxial cliques in a graph. A maximal clique is any clique
-- that can't be enlarged by addition of a node.
cliques :: Graph a -> [[a]]
cliques gr@Graph{..} = do
  cs <-IS.toList <$> cliques' gr
  return $ V.unsafeIndex graphNodes <$> cs 

-- | Find all distinct maxial cliques in a graph. A maximal clique is any clique
-- that can't be enlarged by addition of a node. This returns the indices of the
-- resulting nodes.
cliques' :: Graph a -> [IntSet]
cliques' gr@Graph{..} =
  IS.fromList <$> getMaximalCliques neighboursP
                                    ([0 .. (V.length graphNodes) - 1])
  where
    neighboursP = connected gr
