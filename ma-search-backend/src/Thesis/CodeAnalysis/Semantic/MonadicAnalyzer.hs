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

buildMonadicAnalyzer :: (Monad m) => ([Text] -> [Text] -> m Double)
                     -> SemanticAnalyzer m [Text]
buildMonadicAnalyzer f = SemanticAnalyzer id f
