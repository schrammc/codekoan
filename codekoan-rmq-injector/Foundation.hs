{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
module Foundation where

import Yesod.Core
import Settings
import Settings.LogSettings
import Settings.RabbitMQ
import Control.Concurrent.MVar

import Network.AMQP

import Data.Text (unpack)

data App = App { appRequestCounter :: MVar Int
               , appSettings :: AppSettings
               , appRmqConnection :: Connection
               }

buildFoundation :: AppSettings -> IO App
buildFoundation appSettings@AppSettings{..} = do
  appRequestCounter <- newMVar 0
  appRmqConnection <- connectToRabbitMQ appRmqSettings
  return $ App{..}

connectToRabbitMQ :: RabbitMQSettings -> IO Connection
connectToRabbitMQ RabbitMQSettings{..} =
  openConnection (unpack rmqHost) rmqVirtualHost rmqUser rmqPassword

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  shouldLog App{..} src level =
    let lvl = logSettingsLevel $ appLogSettings appSettings
    in level >= lvl
