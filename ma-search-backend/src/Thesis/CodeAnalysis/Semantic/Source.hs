-- |
-- Author: Christof Schramm, 2016
-- Copyright: All rights reserved
--
-- This module provides conduit access to a few common sources of language text
-- for corpora.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Semantic.Source where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Filesystem
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           System.Directory
import           System.FilePath
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic
import           Thesis.CodeAnalysis.Semantic.IdentifierSplitter

-- | A conduit that recursively traverses a directory and returns all files that
-- have a given extension. This does not follow symlinks.
--
-- This function accepts simple extensions like ".java" and composite extensions
-- like ".tar.gz".
pathsWithExtension :: MonadResource m => String -> FilePath -> Producer m FilePath
pathsWithExtension extension dirpath = sourceDirectoryDeep False dirpath
                                       =$= CL.filter hasExtension
                                       =$= (CL.filter $ \path ->
                                             takeExtensions path == extension)
                                       =$= (CL.mapMaybeM $ \path -> do
                                               e <- liftIO $ doesFileExist path
                                               if e
                                                 then return $ Just path
                                                 else return $ Nothing)

directoryCorpus :: MonadResource m => String -> FilePath -> Producer m Text
directoryCorpus extension dirpath =
  pathsWithExtension extension dirpath =$= (CL.mapM $ liftIO . TIO.readFile)

-- | Transform a stream of document texts into a stream of the list of words
-- in the identifiers of the documents interpreted in the given language.
--
-- Weird results may happen if you try to e.g. feed a haskell file to the java
-- language
identifierWords :: MonadResource m => Language t l -> Conduit Text m [Text]
identifierWords lang@Language{..} =
  (CL.mapMaybe $ \txt -> identifierWordsInCode lang $ LanguageText txt)


