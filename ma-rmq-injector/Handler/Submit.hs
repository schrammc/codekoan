-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A handler for submitting json queries to a rabbitmq
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Submit where

import Control.Concurrent.MVar
import Control.Monad.Logger

import Data.Aeson (encode)
import Data.Text
import Data.Monoid

import Foundation
import Settings

import Network.AMQP hiding (Handler, Message)
import qualified Network.AMQP as AMQP

import Settings.RabbitMQ

import Thesis.Messaging.Message
import Thesis.Messaging.Query

import Yesod.Core

postSubmitR :: Handler Value
postSubmitR = do
  query <- requireJsonBody :: Handler Query

  app@App{..} <- getYesod


  -- Generate a new identifier for the submitted query
  qId <- liftIO $ do
    k <- takeMVar appRequestCounter
    putMVar appRequestCounter (k+1)
    return k

  let queryWithId = query{queryId = Just qId}

  $(logInfo) $ pack $ "Parsed query, assigned id " ++ (show qId) ++ "."
  submitToRabbitMQ app queryWithId

  return $ object ["queryId" .= qId]

-- | Submit a query to a RabbitMQ exchange named query. The routing key will be
-- the query's language.
submitToRabbitMQ :: App -> Query -> Handler ()
submitToRabbitMQ App{..} query = do
  rmqChannel <- liftIO $ do
    chan <- openChannel appRmqConnection
    liftIO $ addChannelExceptionHandler chan $ \e ->
      print $ "Exception:" ++ (show e)
    return chan

  msg <- liftIO $ prepareQueryMessage query

  let queryLang = queryLanguage query
      amqpMessage = newMsg{ msgBody = encode msg}
      exchangeName = "queries"

  $(logDebug) $ "Publishing query to RabbitMQ exchange " <> exchangeName

  liftIO $ do
    AMQP.publishMsg rmqChannel exchangeName queryLang amqpMessage
    closeChannel rmqChannel

prepareQueryMessage :: Query -> IO (Message Query)
prepareQueryMessage q =
  buildMessage "rmq-injector" "A user submitted search query" q
