{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module contains code for search indices over arbitrary languages

module Thesis.Search.Index where

import qualified Data.Set as S
import           Thesis.CodeAnalysis.Language
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.BloomFilter
import           Thesis.Search.CompressedTrie as Trie

data SearchIndex t l where
  SearchIndex :: (Ord t, Eq t) => { indexLanguage :: !(Language t l)
                 , indexTrie :: !(CompressedTrie t (S.Set AnswerFragmentMetaData))
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l
