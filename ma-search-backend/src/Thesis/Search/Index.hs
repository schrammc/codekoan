-- |
-- Description: Search indices for arbitrary languages
-- Maintainer: Christof Schramm
--
--This module contains code for search indices over arbitrary languages

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Thesis.Search.Index where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.ST
import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy
import qualified Data.BloomFilter.Hash as BF.Hash
import qualified Data.BloomFilter.Mutable as BF.Mutable
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Hashable (Hashable, hash)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Text (pack)
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.BloomFilter
import           Thesis.Search.CompressedTrie as Trie
import           Thesis.Search.NGrams
import           Thesis.Util.ConduitUtils

data SearchIndex t l ann where
  SearchIndex :: (Ord t, Eq t, Ord ann) =>
                 { indexLanguage :: !(Language t l)
                 , indexTrie :: !(CompressedTrie t (S.Set ann))
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l ann

-- | Build an index for any given programming language implementation.
buildIndex :: ( Eq t, Ord t, Hashable t
              , MonadIO m, MonadLogger m) => 
              Language t l
           -> Source m Answer
              -- ^ A source of answers with ode. To get this use one
              -- of the Thesis.Data.Stackoverflow.Dump.* modules
           -> Int -- ^ NGram size
           -> m (SearchIndex t l AnswerFragmentMetaData)
buildIndex lang postSource ngramSize = do
  $(logInfo) $ "Building an index for " <> (languageName lang)

  let (bloomSize, bloomNumberFs) = BF.Easy.suggestSizing 1000000 0.01
      hashF = BF.Hash.hashes bloomNumberFs  

  mutableBF <- liftIO $ stToIO $ BF.Mutable.new hashF bloomSize
  
  tr <- postSource
      $$ everyN 25000 (\n ->
                         $(logDebug) $ pack $ "Build index processed " ++
                                              (show n) ++ "answers")
      =$= (CL.map $ \Answer{..} -> do
              (code,n) <- zip (readCodeFromHTMLPost answerBody) [0 ..]
              return $ (code, AnswerFragmentId answerId n))
      =$= CL.concat
      =$= (CL.map $ \(c, aId) -> do
              tokenV <- processAndTokenize lang (LanguageText c)
              Just $ (token <$> tokenV,AnswerFragmentMetaData aId (V.length tokenV))
          )
      =$= CL.catMaybes
      =$ (CL.foldM (\trie -> \(str, v) -> do
                       mapM_ (liftIO . stToIO . BF.Mutable.insert mutableBF) $
                         (hash <$> ngrams ngramSize (V.toList str))
                       return $ mergeTries trie
                                  (buildSuffixTrie (Just ngramSize) str (S.singleton v))
                   )
                   Trie.empty
         )

  bf <- BloomFilter <$> (liftIO . stToIO $ BF.freeze mutableBF)
  return $ SearchIndex lang tr bf ngramSize

