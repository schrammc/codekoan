{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Submit where

import Foundation
import Yesod.Core

import Thesis.Messaging.Query

import Control.Concurrent.MVar
import Control.Monad.Logger

import Data.Text

postSubmitR :: Handler Value
postSubmitR = do
  query <- requireJsonBody :: Handler Query
  App{..} <- getYesod
  n <- liftIO $ do
    k <- takeMVar appRequestCounter
    putMVar appRequestCounter (k+1)
    return k
  $(logInfo) $ pack $ "Submitted a query with id " ++ (show n) ++ "."
  return $ object ["queryId" .= n]
