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

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class

import           Data.Aeson
import           Data.Maybe (fromJust)
import           Data.Text (pack)

import qualified Network.AMQP as AMQP
import           Network.HTTP.Simple

import           Thesis.CodeAnalysis.Language

import           Thesis.Messaging.Message
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import           Thesis.Messaging.SemanticQuery

import           Thesis.Search
import           Thesis.SearchService.ApplicationType
import           Thesis.SearchService.ServiceSettings
import           Thesis.CodeAnalysis.Semantic.MonadicAnalyzer

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
appLoop :: (MonadIO m, MonadLogger m) => Application m -> AMQP.Channel -> m ()
appLoop foundation@(Application{..}) channel = do
  $(logDebug) "Listeing for queries..."

  (amqpMessage, envelope) <- getMessage 0

  case decode $ AMQP.msgBody amqpMessage of
    -- Just log an error if we can't decode the amqp-message
    Nothing -> do
      $(logError) "Failed to parse amqpMesage."
    Just (message :: Message Query) -> do
      let MessageHeader{..} = messageHeader message
          Query{..} = messageContent message
      $(logInfo) $ pack $ "Received query (" ++ (show queryId) ++
                          ") from " ++ show headerSender

      let langText = LanguageText queryText

      searchResult <- performSearch appIndex
                                    appLanguage
                                    appDictionary
                                    querySettings
                                    langText
                                    remoteAnalyzer

      case searchResult of
        -- Log an error if we can't find a search result in the index for the query
        Nothing -> $(logError) $ pack $
                     "Failed to produce a result for query" ++ show queryId
        Just matches -> do
          $(logInfo) $ pack $ "Sending reply to query " ++ show queryId ++
                              " back to rabbitmq..."

          let reply =
                resultSetToMsg (serviceClusterSize appSettings)(languageName appLanguage) (fromJust queryId) matches

          -- Send the reply to the replies queue in rabbitmq
          replyMessage <-  liftIO $ buildMessage "search service" "reply" reply

          liftIO $ AMQP.publishMsg channel "replies" ""
            AMQP.newMsg{AMQP.msgBody = encode replyMessage}

  $(logDebug) "Acknowledging message..."
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
    remoteAnalyzer = buildMonadicAnalyzer getSimilarity
    getSimilarity a b = do
      let submitR = setRequestMethod "POST" $
                    setRequestBodyJSON (SemanticQuery a b) $ 
                            parseRequest_ (serviceSemanticURL appSettings)
      resp <- httpJSON submitR
      return $ getResponseBody resp

  
-- | Get a value from a 'Maybe' or throw an 'error' with the given string.
getOrFail :: (MonadLogger m) => String -> (Maybe a) -> m a
getOrFail str Nothing  = do
  $(logErrorSH) str
  error str
getOrFail _   (Just x) = return x
