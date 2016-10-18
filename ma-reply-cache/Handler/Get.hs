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

  let resultValue :: Maybe (Either Text.Text (ResultSetMsg, Text.Text))
      resultValue = do
        val <- M.lookup qId cacheContent
        case val of
          Left reason -> return $ Left (Text.pack reason)
          Right values -> do
            merged <- mergeResultSetMsgs values
            if resultNumber merged  == resultClusterSize merged
              then return $ Right (merged , "finished")
              else return $ Right (merged, "pending")
  case resultValue of
    Nothing -> return $ object [ "result" .= Null
                               , "status" .= ("nothing" :: Text.Text)]
    Just (Right (v, msg)) ->
      return $ object ["result" .= v, "status" .= msg]
    Just (Left reason) ->
      return $ object [ "status" .= ("exception" :: Text.Text)
                      , "result" .= reason]
