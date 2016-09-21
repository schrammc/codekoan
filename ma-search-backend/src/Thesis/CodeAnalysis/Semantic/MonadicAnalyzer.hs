-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides a 'SemanticAnalyzer' implementation that works on 'Text'
-- lists in a generic 'Monad'.
--
module Thesis.CodeAnalysis.Semantic.MonadicAnalyzer where

import Data.Text
import Thesis.CodeAnalysis.Semantic

buildIOAnalyzer :: (Monad m) => ([Text] -> [Text] -> m Double)
                -> SemanticAnalyzer m [Text]
buildIOAnalyzer f = SemanticAnalyzer id f
