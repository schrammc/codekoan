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
import           Thesis.CodeAnalysis.Language.Haskell

import           Thesis.Data.Stackoverflow.Dump.Source.Postgres
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Dictionary.Postgres

import           Thesis.Search.Index
import           Thesis.Search.FragmentData

import           Control.DeepSeq

import Data.Hashable (Hashable)
data Application m where
  Application :: (MonadThrow m, MonadIO m, NFData t, Hashable t, Ord t) =>
                 { appRabbitConnection :: !Connection
                 , appSettings         :: !ServiceSettings
                 , appQueue            :: !Text.Text
                 , appGetDictionary       :: m (DataDictionary m)
                 , appLanguage         :: Language t l
                 , appIndex            :: SearchIndex t l AnswerFragmentMetaData
                 }  -> Application m

buildFoundation :: forall m . (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
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
  let appGetDictionary = postgresDictionary serviceDBConnectInfo

  let buildApp :: forall t l . (Ord t, Hashable t, NFData t) =>
                  Language t l
               -> SearchIndex t l AnswerFragmentMetaData
               -> Application m
      buildApp = Application conn settings serviceExchange appGetDictionary

  let filteredAnswerSource =
        answersWithTags psqlConnection [serviceQuestionTag]
        =$= (CL.filter $ \Answer{..} ->
              let lastDigit = (answerIdInt answerId) `mod` 10
              in lastDigit `elem` serviceAnswerDigits
              )

  -- Building language specifics
  if | serviceLanguage == languageName java -> do
         $(logInfo) "Application language: java"
         index <- buildIndexForJava filteredAnswerSource 10
         return $ buildApp java index

     | serviceLanguage == languageName python -> do
         $(logInfo) "Application language: python"
         index <- buildIndexForPython filteredAnswerSource 10
         return $ buildApp python index
     | serviceLanguage == languageName haskell -> do
         $(logInfo) "Application language: haskell"
         index <- buildIndexFromAnswers haskell filteredAnswerSource 10
         return $ buildApp haskell index
     | otherwise -> do
         $(logError) $ "Unrecognized language: " <> serviceLanguage
         throwM InitializationException
