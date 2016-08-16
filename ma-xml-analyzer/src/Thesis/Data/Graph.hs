{-# LANGUAGE RecordWildCards #-}
module Thesis.Data.Graph where

import Data.List (groupBy)

import Data.Vector ((!?))
import qualified Data.Vector as V
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- | Build a bidirectional graph from a vector of vertices and a list of edges
-- that are given as pairs of indices.
buildGraph :: V.Vector a -> [(Int, Int)] -> Graph a
buildGraph graphVertices edges = Graph{..}
  where
    graphEdges = IM.fromList $ do
      edgeGroup <- groupBy (\(a, _) -> \(b, _) -> a == b) preparedEdges
      let edgeSource = fst $ head edgeGroup
          edgeTargets = IS.fromList $ snd <$> edgeGroup
      return (edgeSource, edgeTargets)
    
    preparedEdges = formatEdge <$> filter (\(a,b) -> a /= b
                                                     && a < V.length graphVertices
                                                     && b < V.length graphVertices                                                      && a >= 0 && b >= 0
                                          ) edges
    formatEdge (a,b) = (min a b, max a b)


data Graph a = Graph { graphNodes :: V.Vector a
                     , graphEdges :: IntMap IntSet
                     }

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
