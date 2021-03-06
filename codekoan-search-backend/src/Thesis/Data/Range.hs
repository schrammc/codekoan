-- |
-- Description: Clustering of patterns
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
-- Integer ranges with a phantom type to specify, what data the range actually
-- covers.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
module Thesis.Data.Range
        ( -- * Ranges
          Range(..)

        , rangeLength

          -- * Overlapping and sub-ranges
          -- ** Overlap detection
        , overlap
        , overlapOrBorder

          -- ** Sub-range detection
        , isSubRangeOf

          -- * Unions
        , rangeCover
        , merge

          -- * Coverages
        , coveragePercentage

          -- * Type conversion
        , convertRange

          -- * Specialized accessors
        , textInRange
        , textLinesInRange
        , textInRanges
        , vectorInRange


         ) where

import Data.List (sort, nub, foldl')

import Data.Aeson
import GHC.Generics(Generic)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Vector as V
import Control.DeepSeq
import Data.Coerce (coerce)

-- | An integer range with a phantom type @a@, that allows us to specify, /what/
-- the range pertains to. So e.g. a range in a @[a]@ would be a @'Range' a@.
data Range a = Range { rangeStart :: {-# UNPACK #-} !Int
                     , rangeEnd :: {-# UNPACK #-} !Int
                     }
           deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)


-- | The number of characters in a range
rangeLength :: Range a -> Int
rangeLength (Range a b) = b - a
{-# INLINE rangeLength #-}

-- | From a given list of ranges build a list of ranges so that the ranges are
-- non-overlapping (i.e. no position is covered by a range twice). The resulting
-- ranges are sorted in order of occurrence.
--
-- If two ranges are conflicting, they are merged into one range covering both
-- original ranges
--
-- /This assumes that the input ranges are sorted!/
rangeCover :: [Range a] -> [Range a]
rangeCover ranges =
  case ranges of
    r:rs -> result r rs
    [] -> []
  where
    result cur [] = [cur]
    result cur (range:ranges) =
      case merge cur range of
        Just r  -> result r ranges
        Nothing -> cur:(result range ranges)

-- | From a given list of ranges build a list of ranges so that the ranges are
-- non-overlapping (i.e. no position is covered by a range twice). Furthermore the
-- resulting ranges are sorted in order of occurrence.
--
-- If two ranges are conflicting, the result will contain two or three ranges,
-- that cover the overlapping and the one (or two) non overlapping parts of the
-- original ranges.
--
-- Example:
--
-- @
-- A:   |------------|
-- B:          |-------------|
-- Result:
--      |------|-----|-------|
-- @
rangeSplits :: [Range a] -> [Range a]
rangeSplits rangeList =
  case rangeList of
    []  -> rangeList
    _   -> result (head pointsList) (tail pointsList)
  where
    (starts, stops) = unzip $ (\(Range a b) -> (a,b)) <$> rangeList
    pointsList = nub $ sort $ starts ++ stops
    result _ [] = []
    result !x (p:ps) = (Range x p):(result p ps)


-- | Given a set of ranges and a length of the string that the ranges are drawn
-- from return the fraction of positions that are covered by at least one range.
coveragePercentage :: Int -> [Range a] -> Double
coveragePercentage !n ranges =
  (fromIntegral $ foldl' (+) 0 $ rangeLength <$> rangeCover ranges) / (fromIntegral n)

-- | Merge two ranges. If the two ranges overlap or border each other, return a
-- merged range including both. If they don't, return Nothing.
merge :: Range a -> Range a -> Maybe (Range a)
merge ra@(Range a b) rb@(Range c d) | overlapOrBorder ra rb =
                                        Just $! Range (min a c) (max b d)
                                    | otherwise = Nothing
{-# INLINE merge #-}

-- | A predicate to tell if two ranges overlap. Overlapping is when two ranges
-- share at least one position.
--
-- Edge cases: Empty ranges don't overlap overlap, i.e.
-- @
-- overlap (Range 0 0) (Range 0 0) == False
-- @
overlap :: Range a -> Range a -> Bool
overlap !(Range a b) !(Range c d) =
  (b <= d && c < b) || (d <= b && a < d)
{-# INLINE overlap #-}

-- | A predicate to tell if two ranges overlap or touch.
overlapOrBorder :: Range a -> Range a -> Bool
overlapOrBorder !(Range a b) !(Range c d) =
  (b <= d && c <= b) || (d <= b && a <= d)
{-# INLINE overlapOrBorder #-}

-- | Return true if the second range contains the first range
isSubRangeOf :: Range a -> Range a -> Bool
isSubRangeOf !(Range a b) !(Range c d) = c <= a && d >= b
{-# INLINE isSubRangeOf #-}

textInRange :: Range a -> Text -> Text
textInRange (Range a b) txt = Text.take (b - a) $! (Text.drop a txt)
{-# INLINE textInRange #-}

-- | A helper function for display purposes. This function returns the text in
-- the given range and the line - range for the covered text (end-exclusive).
--
-- This is primarily a helper function for displaying and should turn out to be
-- a performance problem if used to frequently due to 'Data.Text's linear-time
-- API.
textLinesInRange :: Range a -> Text -> (Text, (Int, Int))
textLinesInRange (Range a b) txt =
  let (beforeA, afterA) = Text.splitAt a txt
      inRangeT = Text.take (b - a) afterA
      startLine = if beforeA == Text.pack ""
                  then 0
                  else (length $ Text.lines beforeA) - 1
      stopLine  = length $ Text.lines inRangeT
      replaceC ' ' = ' '
      replaceC _   = '.'
      preceding = Text.map replaceC $
                  Text.takeWhileEnd (\c -> c /= '\n') beforeA

      precText = preceding <> Text.take (b - a) afterA
  in precText `seq` (precText, (startLine,startLine+stopLine))

-- | A helper function to convert the phantom type of a range
convertRange :: Range a -> Range b
convertRange = coerce
{-# INLINE convertRange #-}

-- | /O(1)/ Get the slice of the vector that's in range.
vectorInRange :: Range a -> V.Vector a -> Maybe (V.Vector a)
vectorInRange (Range a b) vec | b - a >= 0 && b <= V.length vec  =
                                Just $! V.slice a (b-a) vec
                              | otherwise = Nothing
{-# INLINE vectorInRange #-}

-- | Helper function to get all ranges in a text in one pass because text has
-- /O(n)/ random access.
--
-- /TODO/: This is still wrong in cases where one range includes another like
--
-- @
-- textInRanges (Text.pack "0123456789") [Range 2 4, Range 1 7]
-- @
textInRanges :: Text -> [Range Text] -> [Text]
textInRanges t ranges = texts 0 t (sort ranges)
  where
    texts _ _ [] = []
    texts pos t ((Range start stop):rs)  =
      let t' = Text.drop (start-pos) t
          (tkText, restText) = Text.splitAt (stop-start) t'
      in tkText:(texts stop restText rs)

