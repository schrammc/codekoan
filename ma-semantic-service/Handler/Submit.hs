-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A handler for submitting json queries to a rabbitmq
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Submit where

import Control.Concurrent.MVar
import Control.Monad.Logger

import Data.Aeson (encode)
import Data.Text
import Data.Monoid

import Foundation

import Thesis.CodeAnalysis.Semantic
import Thesis.Messaging.Message
import Thesis.Messaging.SemanticQuery

import Yesod.Core
import Yesod.Core.Handler

postSubmitR :: Handler Value
postSubmitR = do
  SemanticQuery{..} <- requireJsonBody

  app@App{..} <- getYesod

  let SemanticAnalyzer{..} = appSemanticAnalyzer

  let sim = semanticSimilarity (semanticPreprocess setOne)
                               (semanticPreprocess setTwo)
  
  return $ object ["similarity" .= sim]
