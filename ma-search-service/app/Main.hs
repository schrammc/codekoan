-- |
-- Author: Christof Schramm
-- License: All rights reserved
--
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Main where

import Data.Aeson
import Data.Text (pack)

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Thesis.SearchService.ServiceSettings
import Thesis.Messaging.Query
import Thesis.Messaging.Message

import qualified Network.AMQP as AMQP

import Thesis.SearchService.ApplicationType

main :: IO ()
main = runStdoutLoggingT $ do
  $(logInfo) "Search service starting..."

  $(logInfo) "Reading settings..."
  settings <-
    lift (readServiceSettings "settings.yaml") >>= 
    getOrFail "Failed to parse settings"

  $(logInfo) "Constructing app-foundation..."
  foundation@(Application{..}) <- buildFoundation settings

  $(logInfo) "Starting listeining for messages..."

  onException (openChannel appRabbitConnection >>= appLoop foundation) $ do
    $(logInfo) "Shutting down"

  return ()

-- | Open a RabbitMQ channel
openChannel :: (MonadIO m, MonadLogger m) => AMQP.Connection -> m AMQP.Channel
openChannel connection = do
  $(logDebug) "Opening rabbitMQ channel..."
  liftIO $ AMQP.openChannel connection


-- | The application's main loop, that never terminates
appLoop :: (MonadIO m, MonadLogger m) => Application -> AMQP.Channel -> m ()
appLoop foundation@(Application{..}) channel = do
  $(logDebug) "Listeing for queries..."

  (amqpMessage, envelope) <- getMessage 0

  case decode $ AMQP.msgBody amqpMessage of
    Just (message :: Message Query) -> do
      let MessageHeader{..} = messageHeader message
          query@(Query{..}) = messageContent message
      $(logInfo) $ pack $ "Received query (" ++ (show queryId) ++
                          ") from " ++ show headerSender
      return ()
    Nothing -> do
      $(logError) "Failed to parse amqpMesage."
  
  liftIO $ AMQP.ackEnv envelope
  
  appLoop foundation channel
  where
    -- | Get a message, if not at first successful, wait increasingly long (up
    -- to 10ms, so as not to block the CPU senslessly).
    getMessage wait = do
      amqpResult <- liftIO $ AMQP.getMsg channel AMQP.Ack appQueue
      case amqpResult of
        Just (msg, envelope) -> do
          $(logDebug) "RabbitMQ - message received."
          return (msg, envelope)
        Nothing -> do
          let newWait = if wait < 10000 then wait + 100 else wait
          liftIO $ threadDelay newWait
          getMessage newWait
      
  
-- | Get a value from a 'Maybe' or throw an 'error' with the given string.
getOrFail :: (MonadLogger m) => String -> (Maybe a) -> m a
getOrFail str Nothing  = do
  $(logErrorSH) str
  error str
getOrFail _   (Just x) = return x
