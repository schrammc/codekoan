-- |
-- 
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains helper data structures that are needed for block analysis
module Thesis.CodeAnalysis.Semantic.BlockData where

import Thesis.Data.Range

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

data BlockDelim = BlockStart
                | BlockEnd
                deriving (Show, Eq)


-- | The minimal number of blocks that you have to go down and up again, to get
-- from one point to another in a piece of code. This always pertains to going from the first to the second point (in order of occurrence).
data BlockRelation = BlockRelation { up   :: {-# UNPACK #-} !Int
                                   , down :: {-# UNPACK #-} !Int}
                   deriving (Show, Eq)

-- | @BlockRelation 0 0@
inSameBlock :: BlockRelation
inSameBlock = BlockRelation 0 0
