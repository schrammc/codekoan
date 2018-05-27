{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Settings.LogSettings where

import Data.Aeson
import Data.Text
import Yesod.Core

data LogSettings = LogSettings {logSettingsLevel :: LogLevel}

instance FromJSON LogSettings where
  parseJSON = withObject "Logsettings" $ \o -> do
    lvl <- o .: "log-level"
    logSettingsLevel <- case readLogLevel lvl of
      Just l -> return l
      Nothing -> fail ""
    return $ LogSettings{..}

readLogLevel :: Text -> Maybe LogLevel
readLogLevel "debug" = Just LevelDebug
readLogLevel "info"  = Just LevelInfo
readLogLevel "error" = Just LevelError
readLogLevel "warn"  = Just LevelWarn
readLogLevel _ = Nothing
