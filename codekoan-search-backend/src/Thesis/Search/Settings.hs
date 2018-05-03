-- |
-- Description: Search Pipeline Configuration Data Structure
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
-- Search pipeline configuration data structure with json instances.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Search.Settings where

import GHC.Generics (Generic)

import Data.Aeson

data SearchSettings =
  SearchSettings { minMatchLength :: !Int
                 , levenshteinDistance :: !Int
                 , coveragePercentage :: Double
                 , blockFiltering :: Bool
                 , semanticThreshold :: Maybe Double
                 , minSumResultLength :: Int
                 }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default settings for high similarity
highSimilarityDef :: SearchSettings
highSimilarityDef = SearchSettings { minMatchLength = 15
                                   , levenshteinDistance = 0
                                   , coveragePercentage = 0.8
                                   , blockFiltering = True
                                   , semanticThreshold = Just 0.6
                                   , minSumResultLength = 30
                                   }
