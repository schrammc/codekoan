{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Settings.RabbitMQ
import Settings.LogSettings
import Data.Aeson
import Data.Yaml

data AppSettings = AppSettings { appRmqSettings :: RabbitMQSettings
                               , appLogSettings :: LogSettings
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appRmqSettings <- o .: "rabbitmq-settings"
    appLogSettings <- o .: "log-settings"
    return AppSettings{..}
                   

readAppSettings :: FilePath -> IO (Maybe AppSettings)
readAppSettings path = decodeFile path
  
