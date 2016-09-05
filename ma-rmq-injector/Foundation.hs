{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
module Foundation where

import Yesod.Core
import Settings
import Settings.LogSettings
import Control.Concurrent.MVar

data App = App { appRequestCounter :: MVar Int
               , appSettings :: AppSettings
               }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  shouldLog App{..} src level =
    let lvl = logSettingsLevel $ appLogSettings appSettings
    in level >= lvl
