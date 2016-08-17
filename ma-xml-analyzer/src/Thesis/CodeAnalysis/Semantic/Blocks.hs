{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Semantic.Blocks where

import Data.Foldable (foldl')
import Thesis.Data.Range

import Thesis.Search.ResultSet
import Thesis.Search.SearchResult

import Thesis.CodeAnalysis.Language.Java
import Thesis.Data.Graph

import Data.Graph
import qualified Data.Map as M

import qualified Data.Vector as V

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

javaBlockData :: V.Vector Token -> V.Vector Token -> BlockData Token
javaBlockData queryTokens fragmentTokens =
  BlockData { queryRelation    = javaRelation queryTokens
            , fragmentRelation = javaRelation fragmentTokens
            , queryBlockString    = javaBlockStringInRegion queryTokens
            , fragmentBlockString = javaBlockStringInRegion fragmentTokens
            }

javaBlockStringInRegion :: V.Vector Token -> Range t -> [BlockDelim]
javaBlockStringInRegion tks (Range{..}) = javaBlockString subSlice
  where
    subSlice = V.unsafeSlice (normalizeV tks rangeStart)
                             (normalizeV tks rangeEnd)
                             tks

javaBlockString :: V.Vector Token -> [BlockDelim]
javaBlockString tks = do
  token <- V.toList tks
  case token of
    TokenLBrace -> return BlockStart
    TokenRBrace -> return BlockEnd
    _ -> []

javaRelation :: V.Vector Token -> Int -> Int -> BlockRelation
javaRelation tks a b | V.length tks == 0 = inSameBlock
                     | a == b = inSameBlock
                     | otherwise = foldl' f inSameBlock relevantRegion
  where
    relevantRegion = V.unsafeSlice x (y-x) tks
    f :: BlockRelation -> Token -> BlockRelation
    f br@BlockRelation{..} TokenLBrace | down > 0 = br{down=down-1}
                                       | otherwise = br{up=up+1}
    f br@BlockRelation{..} TokenRBrace | up > 0 = br{up = up-1}
                                       | otherwise = br{down = down+1}
    f br _ = br

    -- x and y are the minimum of a and b and the maximum of a and b after a
    -- normalization that puts each value in the interval of 
    x = normalizeV tks $ min a b
    y = normalizeV tks $ max a b

normalizeV :: V.Vector a -> Int -> Int
normalizeV vector k = max 0 $ min k (V.length vector - 1)

data BlockData t =
  BlockData { queryRelation :: Int -> Int -> BlockRelation
            , fragmentRelation :: Int -> Int -> BlockRelation
            , queryBlockString :: Range t -> [BlockDelim]
            , fragmentBlockString :: Range t -> [BlockDelim]
            }

blockAccordance :: BlockData t -> SearchResult t l -> SearchResult t l -> Bool
blockAccordance BlockData{..} resA resB =
  queryDist == fragmentDist && blockStringEquality
  where
    queryDist = queryRelation (rangeStart $ resultQueryRange resA)
                              (rangeStart $resultQueryRange resB)
    fragmentDist = fragmentRelation (rangeStart $ resultFragmentRange resA)
                                    (rangeStart $ resultFragmentRange resB)
    blockStringEquality = (queryBlockString $ resultQueryRange resA) ==
                          (queryBlockString $ resultQueryRange resB)

blockAnalysis :: BlockData t
              -> [SearchResult t l] -- ^ result matches
              -> [[SearchResult t l]]
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

resultSetBlockAnalysis :: BlockData t -> ResultSet t l -> ResultSet t l
resultSetBlockAnalysis bd resultSet =
  mapSplitFragmentResults resultSet (\_ -> \_ -> blockAnalysis bd )
