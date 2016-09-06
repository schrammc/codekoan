{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Search.Settings where

import GHC.Generics (Generic)

import Data.Aeson

data SearchSettings =
  SearchSettings { minMatchLength :: Int
                 , coveragePercentage :: Double
                 , blockFiltering :: Bool
                 , semanticThreshold :: Maybe Double
                 }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
