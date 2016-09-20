-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Thesis.SearchService.ApplicationType where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Catch

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Monoid ((<>))
import qualified Data.Text as Text

import qualified Database.PostgreSQL.Simple as PSQL

import           Network.AMQP
import           Thesis.SearchService.ServiceSettings
import           Thesis.SearchService.InitializationException

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java
import           Thesis.CodeAnalysis.Language.Python

import           Thesis.Data.Stackoverflow.Dump.Source.Postgres
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Dictionary.Postgres

import           Thesis.Search.Index
import Data.Hashable (Hashable)
data Application m where
  Application :: (MonadThrow m, MonadIO m, Hashable t, Ord t) =>
                 { appRabbitConnection :: !Connection
                 , appSettings         :: !ServiceSettings
                 , appQueue            :: !Text.Text
                 , appDictionary       :: DataDictionary m
                 , appLanguage         :: Language t l
                 , appIndex            :: SearchIndex t l
                 }  -> Application m

buildFoundation :: forall m . (MonadThrow m, MonadIO m, MonadLogger m)
                   => ServiceSettings
                -> m (Application m)
buildFoundation settings@ServiceSettings{..} = do

  $(logInfo) $ "Connecting to RabbitMQ on host: " <> rmqHost serviceRMQSettings
  conn <- liftIO $ openConnection (Text.unpack $ rmqHost serviceRMQSettings)
                                  (rmqVirtualHost serviceRMQSettings)
                                  (rmqUser serviceRMQSettings)
                                  (rmqPassword serviceRMQSettings)


  $(logInfo) "Connecting to postgresql..."
  psqlConnection <- liftIO $ PSQL.connect serviceDBConnectInfo
  $(logInfo) "PostgreSQL connection established!"
  appDictionary <- postgresDictionary psqlConnection

  let buildApp :: forall t l . (Ord t, Hashable t) =>
                  Language t l
               -> SearchIndex t l
               -> Application m
      buildApp = Application conn settings serviceExchange appDictionary

  let filteredAnswerSource =
        answersWithTags psqlConnection [serviceQuestionTag]
        =$= (CL.filter $ \Answer{..} ->
              let lastDigit = (answerIdInt answerId) `mod` 10
              in lastDigit `elem` serviceAnswerDigits
              )

  -- Building language specifics
  if | serviceLanguage == languageName java -> do
         $(logInfo) "Application language: java"

         index <- buildIndexForJava filteredAnswerSource 7
         return $ buildApp java index

     | serviceLanguage == languageName python -> do
         $(logInfo) "Application language: python"
         return $ buildApp python undefined
     | otherwise -> do
         $(logError) $ "Unrecognized language: " <> serviceLanguage
         throwM InitializationException
