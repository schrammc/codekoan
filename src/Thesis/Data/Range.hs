module Thesis.Data.Range where

import Data.List (sort, nub)

import Data.Text (Text)
import qualified Data.Text as Text

data Range = Range Int Int
           deriving (Eq, Ord, Show)


-- | The number of characters in a range
rangeLength :: Range -> Int
rangeLength (Range a b) = b - a

-- | From a given list of ranges build a list of ranges so that the ranges are
-- non-overlapping (i.e. no position is covered by a range twice). Furthermore the
-- resulting ranges are sorted in order of occurrence.
--
-- If two ranges are conflicting, they are merged into one range covering both
-- original ranges
rangeCover :: [Range] -> [Range]
rangeCover rangeList =
  case sort rangeList of
    (r:rs) -> result r rs
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
-- A:   |------------|
-- B:          |-------------|
-- Result:
--      |------|-----|-------|
rangeSplits :: [Range] -> [Range]
rangeSplits rangeList =
  case rangeList of
    []  -> rangeList
    _   -> result (head pointsList) (tail pointsList)
  where
    (starts, stops) = unzip $ (\(Range a b) -> (a,b)) <$> rangeList
    pointsList = nub $ sort $ starts ++ stops
    result _ [] = []
    result x (p:ps) = (Range x p):(result p ps)


-- | Given a set of ranges and a length of the string that the ranges are drawn
-- from return the fraction of positions that are covered by at least one range.
coveragePercentage :: Int -> [Range] -> Double
coveragePercentage n ranges =
  (fromIntegral . sum $ rangeLength <$> rangeCover ranges) / (fromIntegral n)

-- | Merge two ranges. If the two ranges overlap, return a merged range
-- including both. If they don't, return Nothing.
merge :: Range -> Range -> Maybe Range
merge ra@(Range a b) rb@(Range c d) | overlap ra rb =
                                        Just $ Range (min a c) (max b d)
                                    | otherwise = Nothing

-- | A predicate to tell if two ranges overlap
overlap :: Range -> Range -> Bool
overlap (Range a b) (Range c d) =
  (b <= d && c < b) || (d <= b && a < d)

-- | Return true if the second range contains the first range
isSubRangeOf :: Range -> Range -> Bool
isSubRangeOf (Range a b) (Range c d) = c <= a && d >= b
