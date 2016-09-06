-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A type for a query document with a handy builder function
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Messaging.Query
       ( -- * A query type
         Query(..)
         -- * Building a query
       , buildQuery
       )where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson

import Thesis.Search.Settings

-- | A single query document
data Query = Query { queryText :: Text
                     -- ^ Text representation of the query
                   , queryLanguage :: Text
                     -- ^ Programming language of the query
                   , queryId :: Maybe Int
                   , querySettings :: SearchSettings
                     -- ^ Settings for the querie's search
                   }
           deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Build a query for a document plus it's language
buildQuery :: Text -> Text -> SearchSettings -> Query
buildQuery q lang settings = Query q lang Nothing settings
