{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.SurveySettings where

import Data.Text
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import qualified Database.PostgreSQL.Simple as PSQL
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

buildConnectInfo :: SurveySettings -> PSQL.ConnectInfo
buildConnectInfo st =
  PSQL.defaultConnectInfo{ PSQL.connectHost = unpack $ dbHost (dbSettings st)
                         , PSQL.connectUser = unpack $ dbUser (dbSettings st)
                         , PSQL.connectPassword =
                              unpack $ dbPassword (dbSettings st)
                         }
