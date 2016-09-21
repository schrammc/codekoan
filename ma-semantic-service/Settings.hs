{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Settings.LogSettings
import Data.Aeson
import Data.Yaml

data AppSettings = AppSettings { appCorpusDir :: FilePath
                               , appCorpusLanguage :: String
                               , appLogSettings :: LogSettings
                               , appSettingsPort :: Int
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appLogSettings    <- o .: "log-settings"
    appSettingsPort   <- o .: "application-port"
    appCorpusDir      <- o .: "corpus-directory"
    appCorpusLanguage <- o .: "corpus-language"
    return AppSettings{..}
                   

readAppSettings :: FilePath -> IO (Maybe AppSettings)
readAppSettings path = decodeFile path
  
