{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.Messaging.Feedback where

import GHC.Generics (Generic)
import Data.Aeson
import Thesis.Messaging.Query
import Data.Text (Text)

data FeedbackYesNoNeutral = Yes | No | Neutral
  deriving (Eq)

printYesNo :: FeedbackYesNoNeutral -> Text
printYesNo Yes = "yes"
printYesNo No  = "no"
printYesNo Neutral = "neutral"

instance FromJSON FeedbackYesNoNeutral where
  parseJSON (String "yes") = return Yes
  parseJSON (String "neutral") = return Neutral
  parseJSON (String "no") = return No
  parseJSON _ = fail "Can't parse FeedbackYesNoNeutral"

instance ToJSON FeedbackYesNoNeutral where
  toJSON Yes = String "yes"
  toJSON Neutral = String "neutral"
  toJSON No = String "no"

data Feedback = Feedback { queryId :: QueryId
                         , similar :: FeedbackYesNoNeutral
                         , reuse :: FeedbackYesNoNeutral
                         , comment :: Text
                         , source :: Text
                         , language :: Text
                         }
  deriving (Eq, Generic, FromJSON, ToJSON)
