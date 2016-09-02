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

-- Specification:
--
-- An algorithm for falsification of matches based on the hierarchical block
-- structure of code.
--
--
-- Example Cases
--
-- Pattern:
--
-- statement a;
-- statement b;
--
-- Query Document -- should be rejected
--
-- class Foo {
--     function a{
--         statement a;
--     }
--     function b{
--         statement b;
--     }
-- }
--
-- Query Document -- should be accepted
--
-- class Foo {
--     function a {
--         statement b;
--
--         for( ...) {
--             ...
--         }
--
--         statement a;
--     } 
-- }

-- The notion of accordance:
--
-- Accordance is a symmetric relation between search results. Two search results
-- are in accordance if:
-- * their pieces of the query document and their pieces of
--   the answer fragment are in the same 'BlockRelation'.
-- * their pieces of the query document and their pieces of the answer fragment
--   don't subsume each other and aren't equal.
--
-- Via this relationship a graph is built. What we look for are clusters in that
-- graph. A cluster is nothing but a group of search results that are in
-- accordance with each other.

data BlockRelation = BlockRelation {up :: Int, down :: Int}
                   deriving (Eq)

inSameBlock :: BlockRelation
inSameBlock = BlockRelation 0 0

data BlockDelim = BlockStart
                | BlockEnd
                deriving (Show, Eq)


normalizeV :: V.Vector a -> Int -> Int
normalizeV vector k = max 0 $ min k (V.length vector - 1)

data BlockData t =
  BlockData { queryRelation :: Int -> Int -> BlockRelation
            , fragmentRelation :: Int -> Int -> BlockRelation
            , queryBlockString :: Range t -> [BlockDelim]
            , fragmentBlockString :: Range t -> [BlockDelim]
            }

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
