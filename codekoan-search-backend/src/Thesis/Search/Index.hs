-- |
-- Description: Search indices for arbitrary languages
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
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
import           Data.Foldable (toList)
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
import           Thesis.Search.FragmentData
import           Thesis.Search.NGrams
import           Thesis.Util.ConduitUtils

data SearchIndex t l ann where
  SearchIndex :: (Hashable t, Eq t, Ord ann) =>
                 { indexLanguage :: !(Language t l)
                 , indexTrie :: !(CompressedTrie t (S.Set ann))
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l ann

buildIndexFromAnswers ::( Eq t, Hashable t
                        , MonadIO m, MonadLogger m) =>
                        Language t l
                      -> Source m (Answer)
                      -- ^ A source of answers with code. To get this use one of
                      -- the Thesis.Data.Stackoverflow.Dump.* modules
                      -> Int -- ^ NGram size
                      -> m (SearchIndex t l AnswerFragmentMetaData)
buildIndexFromAnswers lang src ngramSize =
  buildIndex lang (src =$= toFragments) ngramSize

toFragments :: MonadIO m
            => Conduit Answer m (Int -> AnswerFragmentMetaData, LanguageText l)
toFragments = (CL.map $ \Answer{..} -> do
                      (code,n) <- zip (readCodeFromHTMLPost answerBody) [0 ..]
                      return $ ( AnswerFragmentMetaData
                                 $ AnswerFragmentId answerId n
                               , LanguageText code))
               =$= CL.concat

fragmentsFromAnswers :: (MonadIO m, Foldable f)
                     => Language t l
                     -> f Answer
                     -> m [(AnswerFragmentMetaData
                           , LanguageText l
                           , TokenVector t l)]
fragmentsFromAnswers lang as = runConduit $
  CL.sourceList (toList as) =$=
  toFragments =$=
  CL.map (\(f,frag) -> case tokenize lang frag of
             Nothing -> Nothing
             Just tks -> Just (f $ V.length tks,frag, tks)) =$=
  CL.catMaybes =$=
  CL.consume

buildTestIndex :: ( Eq t, Hashable t
                  , MonadIO m, MonadLogger m, Foldable f) =>
                  Language t l
               -> f (LanguageText l)
               -- ^ A source of answers with code. To get this use one of the
               -- Thesis.Data.Stackoverflow.Dump.* modules
               -> Int -- ^ NGram size
               -> m (SearchIndex t l (Int,Int))
buildTestIndex lang txts ngramSize =
  buildIndex lang codeSource ngramSize
  where
    codeSource = CL.sourceList xs
    xs = zip ((,) <$> [0..]) $ toList txts

-- | Build an index for any given programming language implementation.
buildIndex :: ( Eq t, Hashable t
              , MonadIO m, MonadLogger m, FragmentData d) =>
              Language t l
           -> Source m (Int -> d, LanguageText l)
              -- ^ A source of of pairs. These pairs contain:
              --  * A function that generates an annotation from the length of a
              --      token vector
              --  * Some code in the given language
           -> Int -- ^ NGram size
           -> m (SearchIndex t l d)
buildIndex lang postSource ngramSize = do
  $(logInfo) $ "Building an index for " <> (languageName lang)

  let (bloomSize, bloomNumberFs) = BF.Easy.suggestSizing 1000000 0.01
      hashF = BF.Hash.hashes bloomNumberFs  

  mutableBF <- liftIO $ stToIO $ BF.Mutable.new hashF bloomSize
  
  tr <- postSource
      $$ everyN 25000 (\n ->
                         $(logDebug) $ pack $ "Build index processed " ++
                                              (show n) ++ " answers")
      =$= (CL.map $ \(buildAnn, langTxt) -> do
              tokenV <- processAndTokenize lang langTxt
              Just $ (token <$> tokenV, buildAnn (V.length tokenV))
          )
      =$= CL.catMaybes
      =$ (CL.foldM (\trie -> \(str, v) -> do
--                       mapM_ (liftIO . stToIO . BF.Mutable.insert mutableBF) $
--                         (hash . V.toList <$> ngrams ngramSize str)
                       return $ (mergeTriesWith S.union) trie
                                  (buildSuffixTrie (Just ngramSize) str (S.singleton v))
                   )
                   Trie.empty
         )

  bf <- BloomFilter <$> (liftIO . stToIO $ BF.freeze mutableBF)
  return $ SearchIndex lang tr bf ngramSize
