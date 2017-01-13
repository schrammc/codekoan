-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains generator for the 'BlockData' data type for all
-- languages, that denote the start of a code block by one token and the end of
-- it by another.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE MultiWayIf#-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language.Internal.StandardTokenBlockAnalysis
       ( standardBlockData
       , flatBlockData
       ) where

import           Data.Foldable (foldl')
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Semantic.BlockData
import           Thesis.CodeAnalysis.Semantic.Blocks
import           Thesis.Data.Range

-- | A 'BlockData' value, that judges everything to be in the same block
--
flatBlockData :: BlockData t
flatBlockData = BlockData { queryRelation       = (\_ _ -> inSameBlock)
                          , fragmentRelation    = (\_ _ -> inSameBlock)
                          , queryBlockString    = const []
                          , fragmentBlockString = const []
                          }

-- | Build a block data object for the tokens of one query document and one
-- answer code pattern.
--
standardBlockData :: (Eq t) =>
                   t
                   -- ^ Token that signifies the start of a code block
                -> t
                -- ^ Token that signifies the end of a code block
                -> V.Vector t
                -- ^ The tokens of the processed and tokenized query document
                -> V.Vector t
                -- ^ The tokens of the processed and tokenized pattern-code
                -> BlockData t
standardBlockData indent unindent queryTokens fragmentTokens =
  BlockData { queryRelation    = blockRelation indent unindent queryTokens
            , fragmentRelation = blockRelation indent unindent fragmentTokens
            , queryBlockString    =
                  blockStringInRegion indent unindent queryTokens
            , fragmentBlockString =
                  blockStringInRegion indent unindent fragmentTokens
            }

blockStringInRegion :: (Eq t) =>
                       t
                    -> t
                    -> V.Vector t
                    -> Range t
                    -> [BlockDelim]
blockStringInRegion indent unindent tks (Range{..}) =
  blockString indent unindent subSlice
  where
    subSlice = V.unsafeSlice a (b - a) tks
    a = normalizeV tks rangeStart
    b = normalizeV tks rangeEnd

blockString :: Eq t => t -> t -> V.Vector t -> [BlockDelim]
blockString indent unindent tks = do
  token <- V.toList tks
  if | token == indent   -> return BlockStart
     | token == unindent -> return BlockEnd
     | otherwise -> []

blockRelation :: Eq t => t -> t -> V.Vector t -> Int -> Int -> BlockRelation
blockRelation indent unindent tks a b | V.length tks == 0 = inSameBlock
                                      | a == b = inSameBlock
                                      | otherwise =
                                          foldl' f inSameBlock relevantRegion
  where
    relevantRegion = V.unsafeSlice x (y-x) tks
--    f :: (Eq t) => BlockRelation -> t -> BlockRelation
    f br@BlockRelation{..} t =
      if | t == indent   -> if | down > 0 -> br{down=down-1}
                               | otherwise -> br{up=up+1}
         | t == unindent -> if | up > 0 -> br{up = up-1}
                               | otherwise -> br{down = down+1}
         | otherwise -> br

    -- x and y are the minimum of a and b and the maximum of a and b after a
    -- normalization that puts each value in the interval of 
    x = normalizeV tks $ min a b
    y = normalizeV tks $ max a b

