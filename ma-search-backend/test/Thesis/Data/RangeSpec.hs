{-# LANGUAGE RankNTypes #-}
module Thesis.Data.RangeSpec where

import Data.Maybe

import Test.Hspec
import Test.QuickCheck

import Thesis.Data.Range

instance Arbitrary (Range a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Range (min a b) (max a b)

spec :: SpecWith ()
spec = do
  describe "Thesis.Data.RangeSpec" $ do
    overlapSymmetry
    overlapReflexive
    overlapImpliesOverlapBorder
--    overlapMergeDefined
    rangeCoverCorrect


-- | The overlap detection function should be symmetrical.
overlapSymmetry :: SpecWith ()
overlapSymmetry = it "overlap is symmetrical" $ property $ \a -> \b ->
  overlap a b == overlap b a

-- | Every range that is non-empty must overlap itself
overlapReflexive :: SpecWith ()
overlapReflexive =
  it "overlap is reflexive (forall a: overlap a a if a nonempty)" $ property $
    \a@(Range s e) -> if s == e
                      then True -- This case is necessary because two ranges
                                -- only overlap if they actually share a
                                -- position. Empty ranges can't overlap
                      else overlap a a

-- | If two ranges are overlapping the overlapBorder predicate must be true
overlapImpliesOverlapBorder :: SpecWith ()
overlapImpliesOverlapBorder = it "overlap implies overlapOrBorder" $ property $
                              \a -> \b -> if overlap a b
                                          then overlapOrBorder a b
                                          else True

-- | A property to make sure, that only overlapping ranges have a defined merge
-- result
overlapMergeDefined :: SpecWith ()
overlapMergeDefined = it "only overlapping ranges have a defined merge result" $
                      property $ \a -> \b -> if overlap a b
                                             then isJust $ merge a b
                                             else isNothing $ merge a b


-- | A property to make sure that the 'rangeCover' function doesn't produce
-- overlapping ranges (as these should be merged)
rangeCoverCorrect :: SpecWith ()
rangeCoverCorrect = it "no overlaps in range cover" $ property rangeCoverCorrectP

-- | 
rangeCoverCorrectP :: forall a. [Range a] -> Bool
rangeCoverCorrectP [] = rangeCover [] == []
rangeCoverCorrectP xs = and $ do
  a <- rangeCover xs
  b <- rangeCover xs
  return $ if a == b
           then True
           else overlap a b == False
