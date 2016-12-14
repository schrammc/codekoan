{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains a function 'buildIndexForJava' that builds an index for
-- java code from a 'Conduit'.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.CodeAnalysis.Language.Java.Internal.Index where


import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Conduit
import           Thesis.CodeAnalysis.Language.Java.Internal
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.Index

buildIndexForJava :: ( MonadIO m
                     , MonadLogger m) => Source m Answer
                     -- ^ A source of answers with java code. To get this use
                     -- one of the Thesis.Data.Stackoverflow.Dump.* modules
                  -> Int -- ^ NGram size
                  -> m (SearchIndex Token Java AnswerFragmentMetaData)
buildIndexForJava = buildIndex java
