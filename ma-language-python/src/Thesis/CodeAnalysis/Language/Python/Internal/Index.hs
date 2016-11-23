-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains a function 'buildIndexForJava' that builds an index for
-- java code from a 'Conduit'.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.CodeAnalysis.Language.Python.Internal.Index where


import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.ST

import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy
import qualified Data.BloomFilter.Hash as BF.Hash
import qualified Data.BloomFilter.Mutable as BF.Mutable

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Hashable (hash, Hashable)
import qualified Data.Set as S
import           Data.Text (pack)
import qualified Data.Vector as V

import           Thesis.CodeAnalysis.Language
--import           Thesis.CodeAnalysis.Language.Python.Internal

import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.BloomFilter
import           Thesis.Search.CompressedTrie as Trie
import           Thesis.Search.Index
import           Thesis.Search.NGrams
import           Thesis.Util.ConduitUtils
import           Thesis.CodeAnalysis.Language.Python.Internal.Tokens
import           Thesis.CodeAnalysis.Language.Python.Internal.Lang

