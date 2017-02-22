{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Application () -- for YesodDispatch instance

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Logger

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

import Foundation
import Network.AMQP hiding (Message)
import Settings
import Thesis.Messaging.ResultSet
import Thesis.Messaging.Message
import Thesis.Messaging.Query
import Yesod.Core

main :: IO ()
main = do
  settingsMaybe <- readAppSettings settingsPath
  case settingsMaybe of
    Nothing -> putStrLn $ "Failed to parse settings file" ++ settingsPath
    Just settings -> do

      foundation <- buildFoundation settings
      forkIO $ runStdoutLoggingT (pollMessages foundation)
      warp (appSettingsPort settings) foundation
  where
    settingsPath = "settings.yaml"    

pollMessages :: (MonadCatch m, MonadIO m, MonadLogger m) => App -> m ()
pollMessages app@App{..} = do
  $(logDebug) "Polling for messages"
  chan <- liftIO $ openChannel appRmqConnection
  catch (go 1000 chan) $
    -- In case the channel gets closed during execution wait 5 seconds and
    -- attempt reconnecting.
    \(ChannelClosedException str) -> do
        $(logError) $ "Channel closed. Reason given: " <> (pack str)
        $(logError) "Will reconnect in 5 seconds..."
        liftIO $ threadDelay $ 5 * 1000 * 1000
        pollMessages app
  where
    go :: (MonadIO m, MonadLogger m) => Int -> Channel -> m ()
    go n chan = do
      maybeMsg <- liftIO $ getMsg chan Ack (appReplyQueue appSettings)
      case maybeMsg of
        Nothing -> return ()
        Just (msg, envelope) -> do
          $(logDebug) "Got a message from RabbitMQ"
          case decode (msgBody msg) of
            Nothing -> case decode (msgBody msg) of
              Nothing ->
                $(logError) $ "Failed to decode message from rabbitmq: " <>
                              (decodeUtf8 $ toStrict $ msgBody msg)
              Just (Message{..} :: Message (QueryId, String)) -> do
                mp <- liftIO $ takeMVar appReplyCache
                let (queryId, str) = messageContent
                liftIO $ putMVar appReplyCache (M.insert queryId (Left str) mp)
            Just (Message{..} :: Message ResultSetMsg) -> do
              let res@ResultSetMsg{..} = messageContent
              $(logDebug) $ "Processing a reply to query " <>
                            (pack $ show resultSetQueryId)
              -- Insert the new result into the cache
              mp <- liftIO $ takeMVar appReplyCache
              let mp' = M.insertWith merge resultSetQueryId (Right [res]) mp
              liftIO $ putMVar appReplyCache mp'
              
          $(logDebug) "Acknowledging rabbitmq message"
          liftIO $ ackEnv envelope
      liftIO $ threadDelay n
      go n chan
    merge (Right xs) (Right ys) = Right $ xs ++ ys
    merge (Left x) __ = Left x
    merge _ (Left x) = Left x
