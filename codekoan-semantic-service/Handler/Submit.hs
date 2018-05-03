-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A handler for submitting json queries to a rabbitmq
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Submit where

import Data.Functor.Identity

import Foundation

import Thesis.CodeAnalysis.Semantic
import Thesis.Messaging.SemanticQuery

import Yesod.Core

postSubmitR :: Handler Value
postSubmitR = do
  SemanticQuery{..} <- requireJsonBody

  App{..} <- getYesod

  let SemanticAnalyzer{..} = appSemanticAnalyzer

  let sim :: Double
      sim = runIdentity $ semanticSimilarity (semanticPreprocess setOne)
                                             (semanticPreprocess setTwo)
  
  return $ toJSON sim
