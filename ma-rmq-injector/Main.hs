{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Settings

import Control.Concurrent.MVar
import Data.Text

main :: IO ()
main = do
  settingsMaybe <- readAppSettings settingsPath
  case settingsMaybe of
    Nothing -> putStrLn $ "Failed to parse settings file" ++ settingsPath
    Just settings -> do
      requestCounter <- newMVar 0
      warp 3000 (App requestCounter settings)
  where
    settingsPath = "settings.yaml"
