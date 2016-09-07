{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.SearchService.ApplicationType where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Catch

import           Data.Monoid ((<>))
import qualified Data.Text as Text

import           Network.AMQP
import           Thesis.ServiceSettings
import           Thesis.SearchService.InitializationException

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java
import           Thesis.CodeAnalysis.Language.Python

import           Thesis.Search.Index

data Application where
  Application :: { appRabbitConnection :: !Connection
                 , appSettings         :: !ServiceSettings
                 , appQueue            :: !Text.Text
                 , appLanguage         :: Language t l
                 , appIndex            :: SearchIndex t l
                 }  -> Application

buildFoundation :: (MonadThrow m, MonadIO m, MonadLogger m)
                   => ServiceSettings
                -> m Application
buildFoundation settings@ServiceSettings{..} = do

  conn <- liftIO $ openConnection (Text.unpack $ rmqHost serviceRMQSettings)
                                  (rmqVirtualHost serviceRMQSettings)
                                  (rmqUser serviceRMQSettings)
                                  (rmqPassword serviceRMQSettings)

  let buildApp :: forall t . forall l .
                  Language t l
               -> SearchIndex t l
               -> Application
      buildApp = Application conn settings "queries-java"

  -- Building language specifics
  case serviceLanguage of
    "java" -> do
      $(logInfo) "Application language: java"
      return $ buildApp java undefined
    "python" -> do
      $(logInfo) "Application language: python"
      return $ buildApp python undefined
    _ -> do
      $(logError) $ "Unrecognized language: " <> serviceLanguage
      throwM InitializationException
