-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contains an implementation of the 'BlockData' data type for the
-- python language.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

module Thesis.CodeAnalysis.Language.Python.Internal.BlockAnalysis where

import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language.Internal.StandardTokenBlockAnalysis
import           Thesis.CodeAnalysis.Language.Python.Internal.Tokens
import           Thesis.CodeAnalysis.Semantic.BlockData (BlockData)
import           Thesis.Util.VectorView

-- | A python implementation of 'standardBlockData'
pythonBlockData :: VectorView PyToken
                   -- ^ Tokens of the query document
                -> VectorView PyToken
                -- ^ Tokens of the code pattern
                -> BlockData PyToken
pythonBlockData queryTokens fragmentTokens =
  standardBlockData PyTokenIndent PyTokenUnindent queryTokens fragmentTokens
