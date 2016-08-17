{-# LANGUAGE GADTs #-}
-- | This module provides a conduit source of 'StackoverflowPost's from
-- a Stackoverflow XML dump
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.Data.Stackoverflow.Dump where

import Thesis.Data.Stackoverflow.StackoverflowPost

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question

import Control.Monad.Trans.Maybe

import Data.Conduit
import Control.Monad.Trans.Resource

import Text.XML.Stream.Parse

-- | Create a source of 'StackoverflowPost's from an XML dump file
postSource :: FilePath -> Source (ResourceT IO) StackoverflowPost
postSource xmlFilePath = parseFile def xmlFilePath =$= parsePosts

-- | A data structure for random access to data from stackoverflow
data StackoverflowAccess m where
  StackoverflowAccess :: (Monad m) =>
                         { getSOQuestion :: QuestionId -> MaybeT m Question
                         , getSOAnswer :: AnswerId -> MaybeT m Answer
                         } ->  StackoverflowAccess m
