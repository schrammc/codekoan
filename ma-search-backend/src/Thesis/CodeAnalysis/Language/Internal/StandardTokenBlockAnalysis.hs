-- |
-- Copyright: Christof Schramm 2016, 2017
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
{-# LANGUAGE BangPatterns #-}
module Thesis.CodeAnalysis.Language.Internal.StandardTokenBlockAnalysis
       ( standardBlockData
       , flatBlockData
       ) where

import           Data.Foldable (foldl', toList)
import           Thesis.CodeAnalysis.Semantic.BlockData
import           Thesis.CodeAnalysis.Semantic.Blocks
import           Thesis.Data.Range
import           Thesis.Util.VectorView

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
                -> VectorView t
                -- ^ The tokens of the processed and tokenized query document
                -> VectorView t
                -- ^ The tokens of the processed and tokenized pattern-code
                -> BlockData t
standardBlockData indent unindent queryTokens =
  let queryPD = preData unindent indent queryTokens
  in \fragmentTokens ->
    let fragmentPD = preData unindent indent fragmentTokens
    in BlockData { queryRelation =
                      blockRelationWithPreData queryPD indent unindent queryTokens
                 , fragmentRelation =
                     blockRelationWithPreData fragmentPD
                                              indent
                                              unindent
                                              fragmentTokens
                 , queryBlockString    =
                       blockStringInRegion indent unindent queryTokens
                 , fragmentBlockString =
                       blockStringInRegion indent unindent fragmentTokens
                 }
  

blockStringInRegion :: (Eq t) =>
                       t
                    -> t
                    -> VectorView t
                    -> Range t
                    -> [BlockDelim]
blockStringInRegion indent unindent tks (Range{..}) =
  blockString indent unindent subSlice
  where
    subSlice = unsafeSliceView a (b - a) tks
    a = normalizeV tks rangeStart
    b = normalizeV tks rangeEnd

blockString :: Eq t => t -> t -> VectorView t -> [BlockDelim]
blockString indent unindent tks = do
  token <- toList tks
  if | token == indent   -> return BlockStart
     | token == unindent -> return BlockEnd
     | otherwise -> []

data RelationTracker = RelationTracker { maxDown :: {-# UNPACK #-} !Int
                                       , current :: {-# UNPACK #-} !Int}

startTracker = RelationTracker 0 0

trackerToRelation :: RelationTracker -> BlockRelation
trackerToRelation RelationTracker{..} =
  BlockRelation {down = abs maxDown, up = current - maxDown}

-- | Generate a pre-processed form of relevant qurey-tokens 
preData :: (Eq t) => t -> t -> VectorView t -> [(Int ,t)]
preData unindent indent v =
  let pd = filter (isIndentT . snd) $ zip [0 ..] (toList v)
  in length pd `seq` pd
  where
    isIndentT t = unindent == t || indent == t

blockRelationWithPreData :: (Eq t) => [(Int, t)] -> t -> t -> VectorView t -> Int -> Int -> BlockRelation
blockRelationWithPreData pd unindent indent tks !a !b =
  let 
    relevantRegion =
      snd <$> (takeWhile ((< y) . fst) $ dropWhile ((< x) . fst) pd)
    x = normalizeV tks $ min a b
    y = normalizeV tks $ max a b
  in 
   blockRelationBasic unindent indent relevantRegion x y

blockRelation :: (Eq t) => t -> t -> VectorView t -> Int -> Int -> BlockRelation
blockRelation indent unindent tks !a !b =
  blockRelationBasic indent unindent relevantRegion x y
  where
    relevantRegion = unsafeSliceView x (y-x) tks
    -- x and y are the minimum of a and b and the maximum of a and b after a
    -- normalization that puts each value in the interval of 
    x = normalizeV tks $ min a b
    y = normalizeV tks $ max a b

-- | Assumes that a <= b or bad things happend
blockRelationBasic :: (Foldable f, Eq t) =>
                      t
                   -> t
                   -> f t
                   -> Int
                   -> Int
                   -> BlockRelation
blockRelationBasic indent unindent tks !a !b
  | length tks == 0 = inSameBlock
  | a == b = inSameBlock
  | otherwise = trackerToRelation $! foldl' f startTracker tks
  where
--    relevantRegion = unsafeSliceView x (y-x) tks
    f = updateTracker indent unindent


updateTracker :: Eq t => t -> t -> RelationTracker -> t -> RelationTracker
updateTracker indent unindent !tr@RelationTracker{..} !t =
  if | t == indent   -> tr{current = current + 1}
     | t == unindent ->
       if current == maxDown
       then tr{ maxDown = maxDown - 1, current = current - 1}
       else tr{ current = current - 1}
     | otherwise -> tr
