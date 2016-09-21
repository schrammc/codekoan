{-# LANGUAGE MultiWayIf#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
module Foundation where

import Yesod.Core
import Settings
import Settings.LogSettings

import Data.Functor.Identity

import Thesis.CodeAnalysis.Semantic
import Thesis.CodeAnalysis.Semantic.Chatter
import Thesis.CodeAnalysis.Language.Java
import Thesis.CodeAnalysis.Language.Python

data App = App { appSemanticAnalyzer :: SemanticAnalyzer Identity TermVector
               , appSettings :: AppSettings
               }

buildFoundation :: AppSettings -> IO App
buildFoundation appSettings@AppSettings{..} = do
  corpus <- if | appCorpusLanguage == "java" ->
                 chatterDirectoryCorpus java appCorpusDir
               | appCorpusLanguage == "python" ->
                 chatterDirectoryCorpus python appCorpusDir
               | otherwise ->
                 error $ "Unrecognized language: " ++ appCorpusLanguage

  let appSemanticAnalyzer = chatterAnalyzer corpus
  return $ App{..}

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  shouldLog App{..} src level =
    let lvl = logSettingsLevel $ appLogSettings appSettings
    in level >= lvl
