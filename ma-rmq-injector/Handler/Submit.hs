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

import Network.AMQP hiding (Handler, Message)
import qualified Network.AMQP as AMQP

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
  submitToRabbitMQ app queryWithId $

  return $ object ["queryId" .= qId]

-- | Submit a query to a RabbitMQ exchange named query. The routing key will be
-- the query's language.
submitToRabbitMQ :: App -> Query -> Handler ()
submitToRabbitMQ App{..} query = do

  -- Open a RabbitMQ channel for submitting a query
  rmqChannel <- do
    logF <- askLoggerIO -- Logger function of this logging monad
    liftIO $ do
      chan <- openChannel appRmqConnection

      -- Log all channel exceptions that we receive
      liftIO $ addChannelExceptionHandler chan $ \e ->
        (flip runLoggingT) logF $
          $(logError) $ pack ("Exception: " ++ (show e))

      return chan

  -- Build a sendable message with a bit of meta information like the
  -- generation-time
  msg <- liftIO $ prepareQueryMessage query

  let queryLang = queryLanguage query
      amqpMessage = newMsg{ msgBody = encode msg}
      exchangeName = "queries"

  $(logDebug) $ "Publishing query to RabbitMQ exchange " <> exchangeName

  -- Publish the message and close the channel
  liftIO $ do
    AMQP.publishMsg rmqChannel exchangeName queryLang amqpMessage
    closeChannel rmqChannel

prepareQueryMessage :: Query -> IO (Message Query)
prepareQueryMessage q =
  buildMessage "rmq-injector" "A user submitted search query" q
