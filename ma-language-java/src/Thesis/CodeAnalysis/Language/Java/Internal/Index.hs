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
module Thesis.CodeAnalysis.Language.Java.Internal.Index where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource

import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy
import qualified Data.BloomFilter.Hash as BF.Hash
import qualified Data.BloomFilter.Mutable as BF.Mutable

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Hashable (hash)
import qualified Data.Set as S
import qualified Data.Vector as V

import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java.Internal
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.BloomFilter
import           Thesis.Search.CompressedTrie as Trie
import           Thesis.Search.NGrams
import           Thesis.Search.Index
import           Thesis.Util.ConduitUtils

buildIndexForJava :: Source (ResourceT IO) Answer
                     -- ^ A source of answers with java code. To get this use
                     -- one of the Thesis.Data.Stackoverflow.Dump.* modules
                  -> Int -- ^ NGram size
                  -> IO (SearchIndex Token Java)
buildIndexForJava postSource ngramSize = do
  let (bloomSize, bloomNumberFs) = BF.Easy.suggestSizing 1000000 0.01
      hashF = BF.Hash.hashes bloomNumberFs  

  mutableBF <- stToIO $ BF.Mutable.new hashF bloomSize
  
  tr <- runResourceT $ do
    postSource
      $$ maxElements (Just 400000)
      =$= everyN 25000 (liftIO . print)
      =$= (CL.map $ \Answer{..} -> do
              (code,n) <- zip (readCodeFromHTMLPost answerBody) [0 ..]
              return $ (code, AnswerFragmentId answerId n))
      =$= CL.concat
      =$= (CL.map $ \(c, aId) -> do
              tokenV <- tokenize java (LanguageText c)
              Just $ (token <$> tokenV,AnswerFragmentMetaData aId (V.length tokenV))
          )
      =$= CL.catMaybes
      =$ (CL.foldM (\trie -> \(str, v) -> do
                       mapM_ (lift . stToIO . BF.Mutable.insert mutableBF) $
                         (hash <$> ngrams ngramSize (V.toList str))
                       return $ mergeTries trie
                                  (buildSuffixTrie (Just 7) str (S.singleton v))
                   )
                   Trie.empty
         )

  bf <- BloomFilter <$> (stToIO $ BF.freeze mutableBF)
  return $ SearchIndex java tr bf ngramSize

