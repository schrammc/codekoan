{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Get where

import Control.Concurrent.MVar
import Foundation
import Yesod.Core
import Thesis.Messaging.Query
import qualified Data.Map.Strict as M

getGetR :: QueryId -> Handler Value
getGetR qId = do
  App{..} <- getYesod

  cacheContent <- liftIO $ readMVar appReplyCache
  return $ object ["result" .= (M.lookup qId cacheContent)]
