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
{-# LANGUAGE BangPatterns #-}
module Thesis.CodeAnalysis.Semantic.Blocks where

import           Control.Monad
import           Control.Monad.Trans.Maybe

import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import qualified Data.Vector as V

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic.BlockData
import           Thesis.Data.Graph
import           Thesis.Data.Range
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.ResultSet
import           Thesis.Util.VectorView

normalizeV :: Foldable f => f a -> Int -> Int
normalizeV vector !k = max 0 $! min k (length vector - 1)

-- | Analyze the block accordance of two alignment matches
blockAccordance :: BlockData t
                -> AlignmentMatch t l ann
                -> AlignmentMatch t l ann
                -> Bool
blockAccordance BlockData{..} resA resB =
     noQueryOverlap
  && noFragOverlap
  && queryDist == fragmentDist
--  && blockStringEquality

  where
    queryDist = queryRelation (rangeStart $! resultQueryRange resA)
                              (rangeStart $! resultQueryRange resB)
    fragmentDist = fragmentRelation (rangeStart $! resultFragmentRange resA)
                                    (rangeStart $! resultFragmentRange resB)
--    blockStringEquality = (queryBlockString $ resultQueryRange resA) ==
--                          (queryBlockString $ resultQueryRange resB)
    noQueryOverlap = not $! overlap (resultQueryRange resA)
                                    (resultQueryRange resB)
    noFragOverlap  = not $! overlap (resultFragmentRange resA)
                                    (resultFragmentRange resB)

-- | Build groups of alignment matches from the original set.
--
-- This function constructs an undirected graph, in which alignment matches are
-- the nodes. Edges in the graph are inserted between alignment matches that are
-- in 'blockAccordance'.
--
-- This function then yields all maximal cliques of that graph.
blockAnalysis :: BlockData t
              -> [AlignmentMatch t l ann] -- ^ result matches
              -> [[AlignmentMatch t l ann]]
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

-- | Build a result set where each result only contains maximal groups
-- of alignment matches that are in accordace.
resultSetBlockAnalysis :: (Monad m, Hashable ann, Eq ann) =>
                          (ann -> MaybeT m (TokenVector t l, LanguageText l))
                          -- ^ Access to source code of fragments
                       -> Language t l
                       -- ^ The language that we work with
                       -> VectorView t
                       -- ^ The token vector of the query document
                       -> ResultSet t l ann
                       -- ^ The result set that is to be analyzed
                       -> MaybeT m (ResultSet t l ann)
resultSetBlockAnalysis getTokenV lang queryTokens ResultSet{..} =
  fmap ResultSet $
    -- Loop over all stackoverflow answers in the map
    ((flip M.traverseWithKey) resultSetMap) $ \ann groups -> do
      -- Get the full tokens of the answer fragment and use them to do
      -- block analysis on each group of answer fragments.
      (fragTks,_)  <- getTokenV ann
      let fragTokens = token <$$$> fragTks
          blockData = buildBlockData fragTokens
          analyzedGroups = concat $ blockAnalysis blockData <$> groups
      return analyzedGroups
  where
    buildBlockData fragTokens = languageGenBlockData lang queryTokens fragTokens
