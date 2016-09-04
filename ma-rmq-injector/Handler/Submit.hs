{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Submit where

import Foundation
import Yesod.Core

import Thesis.Messaging.Query

import Control.Concurrent.MVar

import Data.Text

postSubmitR :: Handler Value
postSubmitR = do
  query <- requireJsonBody :: Handler Query
  App{..} <- getYesod
  n <- liftIO $ do
    k <- takeMVar appRequestCounter
    putMVar appRequestCounter (k+1)
    return k
  return $ object ["queryId" .= n]
