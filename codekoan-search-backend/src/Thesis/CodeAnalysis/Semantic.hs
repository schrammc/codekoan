-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides a data type 'SemanticAnalyzer' to analyze the similairty
-- of words in the identifiers of a piece of code.
--
-- 'resultsWithSimilarity' is a function that calculates a similarity score in
-- the interval @[0;1]@, where 0 is most dissimilar and 1 is maximal detectable
-- similarity of identifier words.


module Thesis.CodeAnalysis.Semantic
       ( SemanticAnalyzer(..)
       , resultsWithSimilarity
       , identifierWordsInCode
       )where

import Thesis.CodeAnalysis.Semantic.Internal
