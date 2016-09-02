-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains an implementation of the 'BlockData' data type for the
-- java language.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language.Java.Internal.BlockAnalysis where

import           Data.Foldable (foldl')
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.CodeAnalysis.Semantic.Blocks
import           Thesis.Data.Range

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
    subSlice = V.unsafeSlice a (b - a) tks
    a = normalizeV tks rangeStart
    b = normalizeV tks rangeEnd

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

