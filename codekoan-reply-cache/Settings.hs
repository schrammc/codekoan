{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Data.Aeson
import Data.Text (Text)
import Data.Yaml

import Settings.LogSettings
import Settings.RabbitMQ

data AppSettings = AppSettings { appRmqSettings :: RabbitMQSettings
                               , appLogSettings :: LogSettings
                               , appSettingsPort :: Int
                               , appReplyQueue :: Text
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appRmqSettings  <- o .: "rabbitmq-settings"
    appLogSettings  <- o .: "log-settings"
    appSettingsPort <- o .: "application-port"
    appReplyQueue   <- o .: "reply-queue"
    return AppSettings{..}
                   

readAppSettings :: FilePath -> IO (Maybe AppSettings)
readAppSettings path = decodeFile path
  
