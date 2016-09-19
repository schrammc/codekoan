-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides internals for the java implementation, that are should
-- not be visible to other modules, except for testing or if you know exactly
-- what you are doing.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Thesis.CodeAnalysis.Language.Java.Internal ( Java
                                                  , java
                                                  , javaBlockData
                                                  ) where

import qualified Data.Text as Text

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.CodeAnalysis.Language.Java.Internal.Parser
import           Thesis.CodeAnalysis.Language.Java.Internal.Type
import           Thesis.CodeAnalysis.Language.Java.Internal.BlockAnalysis

java :: Language Token Java 
java = Language { removeComments = LanguageText
                , normalize = removeImportLines
                , tokenize = tokenizeJ
                , isTokenIdentifier = (== TokenIdentifier)
                , languageFileExtension = ".java"
                , languageName = "java"
                , languageGenBlockData = javaBlockData
                }

-- | Remove all lines starting with import 
removeImportLines :: LanguageText Java -> LanguageText Java
removeImportLines LanguageText{..} =
  LanguageText $ Text.unlines $ filter isImport ls
  where
    isImport = not . (Text.isPrefixOf "import")
    ls = Text.lines langText
