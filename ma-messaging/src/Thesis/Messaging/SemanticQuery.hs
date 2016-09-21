{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Messaging.SemanticQuery where

import Data.Text
import GHC.Generics (Generic)
import Data.Aeson

data SemanticQuery = SemanticQuery { setOne :: [Text]
                                   , setTwo :: [Text]
                                   }
                     deriving (Show, Eq, Generic, FromJSON, ToJSON)
