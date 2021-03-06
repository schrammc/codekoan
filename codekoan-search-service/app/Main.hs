-- |
-- Maintainer: Christof Schramm
-- License   : All rights reserved
--
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import           Data.Time.Clock
import qualified Network.AMQP as AMQP
import           Network.HTTP.Simple
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic.MonadicAnalyzer
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Messaging.Message
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import           Thesis.Messaging.SemanticQuery
import           Thesis.Search
import           Thesis.SearchException
import           Thesis.SearchService.ApplicationType
import           Thesis.SearchService.ServiceSettings
import           Thesis.Util.LoggingUtils
import System.Environment

data Timeout = Timeout

-- | Helper function that always returns a 'Timeout' value.
-- It does so after waiting n minutes.
waitAndTimeout :: (MonadIO m) => Int -> m Timeout
waitAndTimeout minutes = do
  liftIO . threadDelay $ minutes' * 60 * 1000 * 1000
  return Timeout
  where
    minutes' = max 0 minutes

timeoutMinutes :: Int
timeoutMinutes = 5

main :: IO ()
main = runOutstreamLogging $ do
  $(logInfo) "Search service starting..."

  $(logInfo) "Reading settings..."
  settings <-
    lift (readServiceSettings "settings.yaml") >>= 
    getOrFail "Failed to parse settings"

  $(logInfo) "Constructing app-foundation..."
  foundation@(Application{..}) <- buildFoundation settings

  $(logInfo) "Starting listening for messages..."

  replyChan <- newChan
  replyThread appRabbitConnection replyChan

  catchAll (openChannel appRabbitConnection >>=
            appLoop foundation replyChan) $ \e -> do
    $(logError) $ "Fatal Exception: " <> (pack $ show e)
    $(logInfo) "Shutting down"

  return ()

-- | Open a RabbitMQ channel
openChannel :: (MonadIO m, MonadLogger m) => AMQP.Connection -> m AMQP.Channel
openChannel connection = do
  $(logDebug) "Opening rabbitMQ channel..."
  liftIO $ AMQP.openChannel connection

replyThread :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) =>
               AMQP.Connection
            -> Chan (Text, BSL.ByteString)
            -> m ()
replyThread conn replyChan = do
  $(logInfo) "Launching reply thread (stdout)"
  replyAmqpChan <- openChannel conn
  fork $ go replyAmqpChan
  return ()
  where
    go replyAmqpChan = do
      $(logDebug) "Waiting to send reply"
      (destination, body) <- liftIO $ readChan replyChan

      liftIO $ AMQP.publishMsg replyAmqpChan destination ""
              AMQP.newMsg{AMQP.msgBody = body}
      $(logInfo) "Reply sent"
      go replyAmqpChan


-- | The application's main loop, that never terminates
appLoop :: (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m) =>
           Application m
        -> Chan (Text, BSL.ByteString)
        -> AMQP.Channel
        -> m ()
appLoop foundation@(Application{..}) replyChan channel = do
  $(logDebug) "Listening for queries..."

  (amqpMessage, envelope) <- getMessage 0

  let getTokenV = \AnswerFragmentMetaData{..} -> MaybeT $ do
        fragMaybe <- runMaybeT $ getAnswerFragment appDictionary
                                                   appLanguage
                                                   fragmentMetaId
        case fragMaybe of
          Nothing -> do
            $(logError) $ "Failed to get answer fragment " <>
                          (pack $ show fragmentMetaId)
            return Nothing
          Just frag -> return $ Just frag

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

      handle (\(SemanticException m) -> do
               $(logError) "SemanticException during search!"
               liftIO $ sendExceptionMsg queryId m) $ do
        startTime <- liftIO $ getCurrentTime
        searchResult <- race (waitAndTimeout timeoutMinutes) 
                             (tokenizeAndPerformCachedSearch appIndex
                                                             appLanguage
                                                             getTokenV
                                                             querySettings
                                                             langText
                                                             remoteAnalyzer)
        endTime <- liftIO $ getCurrentTime

        $(logInfo) $ "Request for " <> (pack $ show queryId) <> " took " <>
                     (pack . show $ diffUTCTime endTime startTime)
      
        case searchResult of
          -- Log an error if we can't find a search result in the index for the
          -- query
          Left Timeout -> do
            let msg = "Search timeout after " <>
                      (show timeoutMinutes) <> " minutes"
            $(logError) $ pack msg
            liftIO $ sendExceptionMsg queryId msg
          Right Nothing -> $(logError) $ pack $
                       "Failed to produce a result for query" ++ show queryId
          Right (Just matches) -> do
            $(logInfo) $ pack $ "Sending reply to query " ++ show queryId ++
                                " back to rabbitmq..."

            $(logDebug) "Constructing JSON-Serializable result..."
      
            replyEither <- race (waitAndTimeout 1) $
                                resultSetToMsg (serviceClusterSize appSettings)
                                               (languageName appLanguage)
                                               (fromJust queryId)
                                               matches
                                               getTokenV
                                               queryText

            case replyEither of
              Right reply -> do
                $(logDebug) "Building RMQ-Message..."
      
                -- Send the reply to the replies queue in rabbitmq
                replyMessage <- liftIO $ buildMessage "search service"
                                                      "reply"
                                                      reply


                $(logDebug) "Pushing to reply channel"
      
                writeChan replyChan ("replies", encode replyMessage)
              Left _ -> do
                let errorMessage = "Failed to construct serializable in a minute."
                $(logError) errorMessage
                liftIO $ sendExceptionMsg queryId (unpack errorMessage)
            return ()

  $(logDebug) "Acknowledging message..."
  liftIO $ AMQP.ackEnv envelope
  
  appLoop foundation replyChan channel
  where
    -- | Get a message, if not at first successful, wait increasingly long (up
    -- to 10ms, so as not to block the CPU senslessly).
    getMessage waitTime = do
      amqpResult <- liftIO $ AMQP.getMsg channel AMQP.Ack appQueue
      case amqpResult of
        Just (msg, envelope) -> do
          $(logDebug) "RabbitMQ - message received."
          return (msg, envelope)
        Nothing -> do
          let newWait = if waitTime < 10000 then waitTime + 100 else waitTime
          threadDelay newWait
          getMessage newWait
    remoteAnalyzer = buildMonadicAnalyzer getSimilarity
    getSimilarity a b = do
      let submitR = setRequestMethod "POST" $
                    setRequestBodyJSON (SemanticQuery a b) $ 
                            parseRequest_ (serviceSemanticURL appSettings)
      resp <- httpJSON submitR
      return $ getResponseBody resp

    sendExceptionMsg :: Maybe QueryId -> String -> IO ()
    sendExceptionMsg queryId message = do
      let qId = fromMaybe (QueryId (-1)) queryId
      reply <- buildMessage "search service"
                            "reply"
                            (qId, message)
      writeChan replyChan ("replies", encode reply)
      return ()

-- | Get a value from a 'Maybe' or throw an 'error' with the given string.
getOrFail :: (MonadLogger m) => String -> (Maybe a) -> m a
getOrFail str Nothing  = do
  $(logErrorSH) str
  error str
getOrFail _   (Just x) = return x
