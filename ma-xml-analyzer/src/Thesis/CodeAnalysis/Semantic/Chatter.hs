-- |
-- Author: Christof Schramm, 2016
-- License: All rights reserved
--
-- This module provides functionality to build semantic analyzers backed by the
-- chaatter NLP libray.
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Semantic.Chatter where

import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as CL

import           NLP.Similarity.VectorSim
import           NLP.Types

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic
import           Thesis.CodeAnalysis.Semantic.Source

-- | Build a semantic analyzer based on a chatter-corpus.
chatterAnalyzer :: Corpus -> SemanticAnalyzer TermVector
chatterAnalyzer corp =
  SemanticAnalyzer { semanticPreprocess = (mkVector corp) . mkDocument
                   , semanticSimilarity = tvSim
                   }

-- | Build a corpus by recursively searching through the given directory for
-- files with the given language's extension. The matched files are then
-- tokenized and the ranges of text underlying identifier tokens are broken up
-- into words using @Thesis.CodeAnalysis.Semantic.IdentifierSplitter@.
chatterDirectoryCorpus :: Language t l -> FilePath -> IO Corpus
chatterDirectoryCorpus lang@Language{..} dirPath = do
  runResourceT $ do
    directoryCorpus languageFileExtension dirPath
      $$ identifierWords lang
      =$= CL.fold (\ doc corp ->
                    let corp'@(Corpus{corpTermCounts = mp}) = addDocument doc corp
                    in mp `seq` corp'
                  ) (mkCorpus [])
