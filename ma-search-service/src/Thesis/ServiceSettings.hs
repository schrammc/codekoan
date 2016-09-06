-- |
--
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- Settings for the search service that are loaded from a config file at startup
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.ServiceSettings where

import Data.Aeson
import qualified Data.Yaml as Yaml

import Data.Text

-- | Settings for the service that are loaded at startup.
data ServiceSettings = ServiceSettings { serviceLanguage :: Text
                                         -- ^ Programming language that the
                                         -- service operates on.
                                       , serviceQuestionTag :: Text
                                         -- ^ The Stackoverflow tag by which the
                                         -- service locates relevant fragments
                                       , serviceRMQSettings :: RabbitMQSettings
                                         -- ^ RabbitMQ connection settings
                                       }

instance FromJSON ServiceSettings where
  parseJSON = withObject "ServiceSettings" $ \o -> do
    serviceLanguage   <- o .: "search-language"
    serviceQuestionTag <- o .: "search-question-tag"
    serviceRMQSettings <- o .: "search-rabbitmq-settings"
    return ServiceSettings{..}

-- | Settings for connecting to RabbitMQ.
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

-- | Read the service Settings from a 
readServiceSettings :: FilePath -> IO (Maybe ServiceSettings)
readServiceSettings path = Yaml.decodeFile path
