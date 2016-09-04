{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Status where

import Foundation
import Yesod.Core

import Data.Aeson
import Data.Text
import Control.Concurrent.MVar

-- | A handler to get status information about the service. Reply with HTML/JSON
-- depending on the content type in the request header.
getStatusR :: Handler TypedContent
getStatusR = do
  (status, msg) <- liftIO $ getStatus
  App{..} <- getYesod
  n <- liftIO $ readMVar appRequestCounter
  selectRep $ do -- Provide different responses depending on content type
    provideRep $ return [shamlet|
      <h1> RabbitMQ Injector
      Status: #{status} <br>
      Details: #{msg} <br>
      Request count: #{n}
      |]
    provideRep $ returnJson $ object [ "status" .= status
                                     , "message" .= msg
                                     , "requestCound" .= n
                                     ]
  where
    getStatus :: IO (Int, Text) 
    getStatus = return (200, "OK")
  
