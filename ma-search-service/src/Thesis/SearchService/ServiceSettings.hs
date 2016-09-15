-- |
--
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- Settings for the search service that are loaded from a config file at startup
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.SearchService.ServiceSettings where

import           Control.Monad

import           Data.Aeson
import qualified Data.Yaml as Yaml
import           Data.Text hiding (null)
import           Database.PostgreSQL.Simple (ConnectInfo(..))

-- | Settings for the service that are loaded at startup.
data ServiceSettings =
  ServiceSettings { serviceLanguage :: !Text
                    -- ^ Programming language that the service operates on.
                  , serviceExchange :: !Text
                    -- ^ The RabbitMQ exchange that we pull pull requests from
                  , serviceQuestionTag :: !Text
                    -- ^ The Stackoverflow tag by which the service locates
                    -- relevant fragments
                  , serviceRMQSettings :: !RabbitMQSettings
                    -- ^ RabbitMQ connection settings
                  , serviceAnswerDigits :: [Int]
                    -- ^ Answers with what ending-digits in their ID will be
                    -- indexed by this service?
                  , serviceDBConnectInfo :: !ConnectInfo
                  }

instance FromJSON ServiceSettings where
  parseJSON = withObject "ServiceSettings" $ \o -> do
    serviceLanguage         <- o .: "search-language"
    serviceExchange         <- o .: "search-exchange"
    serviceQuestionTag      <- o .: "search-question-tag"
    serviceRMQSettings      <- o .: "search-rabbitmq-settings"
    serviceAnswerDigits     <- o .: "search-answer-digits"
    serviceDBConnectInfo    <- readPostgresConnectInfo o
    when (null serviceAnswerDigits) $ fail "No answer digits specified!"
    return ServiceSettings{..}
    where
      readPostgresConnectInfo o = do
        dbObject <- o .: "search-postgres-database"
        ConnectInfo <$> dbObject .: "db-host"
                    <*> dbObject .: "db-port"
                    <*> dbObject .: "db-user"
                    <*> dbObject .: "db-pwd"
                    <*> dbObject .: "db-name"

-- | Settings for connecting to RabbitMQ.
data RabbitMQSettings =
  RabbitMQSettings { rmqUser        :: !Text
                   , rmqPassword    :: !Text
                   , rmqHost        :: !Text
                   , rmqVirtualHost :: !Text
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
