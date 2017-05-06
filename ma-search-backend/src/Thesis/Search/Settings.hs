{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Search.Settings where

import GHC.Generics (Generic)

import Data.Aeson

data SearchSettings =
  SearchSettings { minMatchLength :: Int
                 , levenshteinDistance :: Int
                 , coveragePercentage :: Double
                 , blockFiltering :: Bool
                 , semanticThreshold :: Maybe Double
                 , minSumResultLength :: Int
                 }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
