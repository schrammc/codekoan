module Thesis.Data.RangeSpec where

import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Thesis.Data.Range

instance Arbitrary Range where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Range (min a b) (max a b)

spec = do
  describe "Thesis.Data.RangeSpec" $ do
    overlapSymmetry
    overlapMergeDefined
    rangeCoverCorrect

overlapSymmetry = it "overlap is symmetrical" $ property $ \a -> \b ->
  overlap a b == overlap b a

overlapMergeDefined = it "only overlapping ranges have a defined merge result" $
                      property $ \a -> \b -> if overlap a b
                                             then isJust $ merge a b
                                             else isNothing $ merge a b



rangeCoverCorrect = it "no overlaps in range cover" $ property rangeCoverCorrectP

rangeCoverCorrectP [] = rangeCover [] == []
rangeCoverCorrectP xs = and $ do
  a <- rangeCover xs
  b <- rangeCover xs
  return $ if a == b
           then True
           else overlap a b == False
