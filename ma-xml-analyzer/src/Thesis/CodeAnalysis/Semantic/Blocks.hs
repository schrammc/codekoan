-- |
-- 
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides functionality to analyze the block accordance of search
-- results. This should be the first starting point in static analysis.
--
-- = Specification:
--
-- An algorithm for falsification of matches based on the hierarchical block
-- structure of code.
--
-- == The notion of accordance:
--
-- Accordance is a symmetric relation between search results. Two search results
-- are in accordance if:
--
--   * their pieces of the query document and their pieces of
--     the answer fragment are in the same 'BlockRelation'.
--
--   * their pieces of the query document and their pieces of the answer
--     fragment don't subsume each other and aren't equal.
--
-- Via this relationship a graph is built. What we look for are clusters in that
-- graph. A cluster is nothing but a group of search results that are in
-- accordance with each other.
--
-- == Example Case for Accordance
--
-- This is a simple example of a trivial pattern and to query document.
--
-- === Pattern:
--
-- @
-- statement a;
-- statement b;
-- @
--
-- === Query Document -- /should be rejected/
--
-- @
-- class Foo {
--     function a{
--         statement a; --|
--     }                  |--- 'BlockRelation' 1 1
--     function b{        |
--         statement b; --|
--     }
-- }
-- @
--
-- === Query Document -- /should be accepted/
--
-- @
-- class Foo {
--     function a {
--         statement b; --| 
--                        |
--         for( ...) {    |
--             ...        |--- 'BlockRelation' 0 0
--         }              |
--                        |
--         statement a; --|
--     } 
-- }
-- @
--
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Semantic.Blocks where

import           Control.Monad
import           Control.Monad.Trans.Maybe

import qualified Data.Map as M
import qualified Data.Vector as V

import           Thesis.CodeAnalysis.Language
import           Thesis.Data.Graph
import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Search.ResultSet
import           Thesis.Search.AlignmentMatch

-- | The minimal number of blocks that you have to go down and up again, to get
-- from one point to another in a piece of code. This always pertains to going from the first to the second point (in order of occurrence).
data BlockRelation = BlockRelation {up :: Int, down :: Int}
                   deriving (Eq)

-- | @BlockRelation 0 0@
inSameBlock :: BlockRelation
inSameBlock = BlockRelation 0 0

data BlockDelim = BlockStart
                | BlockEnd
                deriving (Show, Eq)


normalizeV :: V.Vector a -> Int -> Int
normalizeV vector k = max 0 $ min k (V.length vector - 1)

-- | A helper data structure for block accordance analysis. One value of this
-- type is always generated for /one query document and one code fragment/.
data BlockData t =
  BlockData { queryRelation :: Int -> Int -> BlockRelation
              -- ^ Block - relationship of two points in the query
            , fragmentRelation :: Int -> Int -> BlockRelation
              -- ^ Block - relationship of two points in the fragment
            , queryBlockString :: Range t -> [BlockDelim]
              -- ^ String of block delimiters in the given range in the query
            , fragmentBlockString :: Range t -> [BlockDelim]
              -- ^ String of block delimiters in the given range in the fragment
            }

-- | Analyze the block accordance of two alignment matches
blockAccordance :: BlockData t -> AlignmentMatch t l -> AlignmentMatch t l -> Bool
blockAccordance BlockData{..} resA resB =
  queryDist == fragmentDist
  && blockStringEquality
  && noQueryOverlap
  && noFragOverlap
  where
    queryDist = queryRelation (rangeStart $ resultQueryRange resA)
                              (rangeStart $ resultQueryRange resB)
    fragmentDist = fragmentRelation (rangeStart $ resultFragmentRange resA)
                                    (rangeStart $ resultFragmentRange resB)
    blockStringEquality = (queryBlockString $ resultQueryRange resA) ==
                          (queryBlockString $ resultQueryRange resB)
    noQueryOverlap = not $ overlap (resultQueryRange resA)
                                   (resultQueryRange resB)
    noFragOverlap  = not $ overlap (resultFragmentRange resA)
                                   (resultFragmentRange resB)

-- | Build groups of alignment matches from the original set.
--
-- This function constructs an undirected graph, in which alignment matches are
-- the nodes. Edges in the graph are inserted between alignment matches that are
-- in 'blockAccordance'.
--
-- This function then yields all maximal cliques of that graph.
blockAnalysis :: BlockData t
              -> [AlignmentMatch t l] -- ^ result matches
              -> [[AlignmentMatch t l]]
blockAnalysis blockData results = cliques accordanceGraph
  where
    resultV   = V.fromList results
    accordance = blockAccordance blockData

    accordanceGraph = buildGraph resultV edges
    edges = do
      (n, result ) <- zip [0 ..] results
      (k, result') <- zip [0 .. (max 0 (n-1))] results
      if accordance result result'
        then [(k, n)]
        else []

-- | Build a result set from a result set, that contains results, where each
-- result only contains maximal groups of alignment matches that are in
-- accordace.
resultSetBlockAnalysis :: (Monad m) =>
                          DataDictionary m
                       -> Language t l
                       -> ResultSet t l
                       -> (V.Vector t -> V.Vector t -> BlockData t)
                       -- ^ A function that takes the vector of query doc tokens
                       -- and the vector of answr fragment tokens and builds a
                       -- 'BlockData' object.
                       -> V.Vector t
                       -> MaybeT m (ResultSet t l)
resultSetBlockAnalysis dict lang ResultSet{..} mkBlockData queryTokens = do
  updatedLst <- forM (M.toList resultSetMap) $ \(aId, fragMap) -> do
    fragMap' <- forM (M.toList fragMap) $ \(fragId, groups) -> do
      (fragTks,_)  <- answerFragmentTokens dict lang (AnswerFragmentId aId fragId)
      let fragTokens = token <$> fragTks
          blockData = mkBlockData queryTokens fragTokens
          analyzedGroups = concat $ blockAnalysis blockData <$> groups
      return (fragId, analyzedGroups)
    return (aId, M.fromList fragMap')
  return $ ResultSet (M.fromList updatedLst)
