{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.SearchService.ApplicationType where

import           Control.Monad.IO.Class
import           Control.Monad.Logger

import qualified Data.Text as Text

import           Network.AMQP
import           Thesis.ServiceSettings

data Application =
  Application{ appRabbitConnection :: !Connection
             , appSettings         :: !ServiceSettings
             , appQueue            :: !Text.Text
             }

buildFoundation :: (MonadIO m, MonadLogger m) => ServiceSettings -> m Application
buildFoundation settings@ServiceSettings{..} = do
  conn <- liftIO $ openConnection (Text.unpack $ rmqHost serviceRMQSettings)
                                  (rmqVirtualHost serviceRMQSettings)
                                  (rmqUser serviceRMQSettings)
                                  (rmqPassword serviceRMQSettings)
  return $ Application conn settings "queries-java"
