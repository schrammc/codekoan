-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains an implementation of the 'BlockData' data type for the
-- java language.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language.Java.Internal.BlockAnalysis where

import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language.Internal.StandardTokenBlockAnalysis
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.CodeAnalysis.Semantic.BlockData (BlockData)
import           Thesis.Util.VectorView

-- | A java implementation of 'standardBlockData'
javaBlockData :: VectorView Token
              -> VectorView Token
              -> BlockData Token
javaBlockData = standardBlockData TokenLBrace TokenRBrace
