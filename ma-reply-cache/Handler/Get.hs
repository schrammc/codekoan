{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Get where

import Control.Concurrent.MVar
import qualified Data.Text as Text
import Foundation
import Yesod.Core
import Thesis.Messaging.Query
import Thesis.Messaging.ResultSet
import qualified Data.Map.Strict as M

getGetR :: QueryId -> Handler Value
getGetR qId = do
  App{..} <- getYesod

  cacheContent <- liftIO $ readMVar appReplyCache

  let resultValue :: Maybe (ResultSetMsg, Text.Text)
      resultValue = do
        val <- M.lookup qId cacheContent
        let n = length val
            clusterSize = resultClusterSize $ head val
        if n == clusterSize
          then mergeResultSetMsgs val >>= return . (, "finished")
          else mergeResultSetMsgs val >>= return . (, "pending")
  case resultValue of
    Nothing -> return $ object [ "result" .= Null
                               , "status" .= ("nothing" :: Text.Text)]
    Just (v, msg) ->
      return $ object ["result" .= v, "status" .= msg]
