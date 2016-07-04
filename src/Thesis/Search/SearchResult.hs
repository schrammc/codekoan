module Thesis.Search.SearchResult where

import Thesis.Data.Range
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Text.PositionRange

data SearchResult t  =
  SearchResult { resultTextRange :: PositionRange
               , resultMatchedTokens :: [t]
               , resultMetaData :: AnswerFragmentMetaData
               , resultFragmentRange :: Range
               , resultLevenScore :: Int
               }
  deriving (Eq, Show)
