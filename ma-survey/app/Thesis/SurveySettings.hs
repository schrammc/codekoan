{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.SurveySettings where

import Data.Text
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data SurveyDbSettings = SurveyDbSettings { dbPassword :: Text
                                         , dbUser :: Text
                                         , dbHost :: Text
                                         }
                        deriving (Show, Generic, FromJSON)

data SurveySettings = SurveySettings { dbSettings :: SurveyDbSettings
                                     , submitUrl :: Text
                                     , getUrl :: Text
                                     }
                    deriving (Show, Generic, FromJSON)
