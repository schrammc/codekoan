{-# LANGUAGE MultiParamTypeClasses #-}
module Thesis.CodeAnalysis.Mutation.Class where

import Data.Text (Text)
import Thesis.CodeAnalysis.Language
import Thesis.Data.Range

data GenericIndentation = GenericIndent
                        | GenericUndindent

class MutableLanguage t l where
  mutableBaseLanguage :: Language t l
  statementRanges :: Language t l -> LanguageText l -> [Range Text]
  hasRelevantIndents :: Language t l -> Bool
  isRelevantIndent :: Language t l -> t -> Maybe GenericIndentation
