{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module contains code for search indices over arbitrary languages

module Thesis.Search.Index where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource

import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy
import qualified Data.BloomFilter.Hash as BF.Hash
import qualified Data.BloomFilter.Mutable as BF.Mutable

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Hashable (Hashable, hash)
import qualified Data.Set as S
import qualified Data.Vector as V

import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Dump
import           Thesis.Data.Stackoverflow.StackoverflowPost
import           Thesis.Search.BloomFilter
import           Thesis.Search.CompressedTrie as Trie
import           Thesis.Search.NGrams

import Control.DeepSeq

data SearchIndex t l where
  SearchIndex :: (Ord t, Eq t) => { indexLanguage :: !(Language t l)
                 , indexTrie :: !(CompressedTrie t (S.Set AnswerFragmentMetaData))
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l

buildIndexForJava :: DataDictionary -- ^ The data dictionary
                  -> FilePath -- ^ Path to the file with the posts
                  -> Int      -- ^ NGram size
                  -> IO (SearchIndex Token Java)
buildIndexForJava dict postsFile ngramSize = do
  let (bloomSize, bloomNumberFs) = BF.Easy.suggestSizing 1000000 0.01
      hashF = BF.Hash.hashes bloomNumberFs  

  mutableBF <- stToIO $ BF.Mutable.new hashF bloomSize
  
  nVar <- newMVar (0 :: Integer)
  
  tr <- runResourceT $ do
    postSource postsFile
      $$ (CL.iterM $ \_ -> lift $ do -- Print a message every 25000 posts to
                                     -- show progress
              n <- takeMVar nVar
              putMVar nVar $! (n+1)
              when (n `mod` 25000 == 0) $ putStrLn $ show n
          )
      =$= filterAnswers
      =$= (CL.filter $ \Answer{..} -> answerWithTag dict "java" answerId)
      =$= (CL.map $ \Answer{..} -> do
              (code,n) <- zip (readCodeFromHTMLPost answerBody) [0 ..]
              return $ (code, AnswerFragmentId answerId n))
      =$= CL.concat
      =$= (CL.map $ \(c, aId) -> do
              tokenV <- buildTokenVector java (LanguageText c)
              Just $ (tokenV,AnswerFragmentMetaData aId (V.length tokenV))
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
  return $ SearchIndex java (force tr) bf ngramSize

