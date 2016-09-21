{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings.RabbitMQ where

import Data.Text
import Data.Aeson

data RabbitMQSettings =
  RabbitMQSettings { rmqUser        :: Text
                   , rmqPassword    :: Text
                   , rmqHost        :: Text
                   , rmqVirtualHost :: Text
                   }

instance FromJSON RabbitMQSettings where
      parseJSON = withObject "RabbitMQSettings" $ \o -> do
        rmqUser        <- o .: "rabbitmq-user"
        rmqPassword    <- o .: "rabbitmq-pwd"
        rmqHost        <- o .: "rabbitmq-host"
        rmqVirtualHost <- o .: "rabbitmq-virtual-host"
        return RabbitMQSettings{..}
