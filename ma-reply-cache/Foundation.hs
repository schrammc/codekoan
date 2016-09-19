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
import Thesis.Messaging.ResultSet
import Thesis.Messaging.Query
import qualified Data.Map as M
import Network.AMQP

import Data.Text (unpack)

instance PathPiece QueryId where
  fromPathPiece t = QueryId <$> (fromPathPiece t)
  toPathPiece (QueryId n) = toPathPiece n

data App = App { appReplyCache :: MVar (M.Map QueryId [ResultSetMsg])
               , appSettings :: AppSettings
               , appRmqConnection :: Connection
               }

buildFoundation :: AppSettings -> IO App
buildFoundation appSettings@AppSettings{..} = do
  appReplyCache <- newMVar M.empty
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
