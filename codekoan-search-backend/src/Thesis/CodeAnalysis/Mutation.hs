module Thesis.CodeAnalysis.Mutation
       ( module Thesis.CodeAnalysis.Mutation.Base
       , module Thesis.CodeAnalysis.Mutation.Class
       , module Thesis.CodeAnalysis.Mutation.IdentifierReplacement

       , mutateOnceAndSelfSearch
       , mutateOnceAndSearchAgainst
       ) where

import Thesis.CodeAnalysis.Mutation.IdentifierReplacement
import Thesis.CodeAnalysis.Mutation.Class
import Thesis.CodeAnalysis.Mutation.Base

import Control.DeepSeq (NFData)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Random
import Data.Hashable
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Semantic
import Thesis.Search
import Thesis.Search.Index
import Thesis.Search.ResultSet
import Thesis.Search.Settings

mutateOnceAndSelfSearch :: (Hashable t, Eq t, NFData t
                           , MonadIO m, MonadLogger m, MonadRandom m, MonadThrow m)
                    => Language t l
                    -> (LanguageText l -> m (LanguageText l))
                    -> SemanticAnalyzer m a
                    -> SearchSettings
                    -> LanguageText l
                    -> m (Bool, LanguageText l)
mutateOnceAndSelfSearch lang mutate analyzer settings txt = do
  mutateOnceAndSearchAgainst lang mutate analyzer settings txt txt

mutateOnceAndSearchAgainst :: (Hashable t, Eq t, NFData t
                           , MonadIO m, MonadLogger m, MonadRandom m, MonadThrow m)
                    => Language t l
                    -> (LanguageText l -> m (LanguageText l))
                    -> SemanticAnalyzer m a
                    -> SearchSettings
                    -> LanguageText l
                    -> LanguageText l
                    -> m (Bool, LanguageText l)
mutateOnceAndSearchAgainst lang mutate analyzer settings query ind = do
  testIndex <- buildTestIndex lang [ind] 10
  
  mutatedText <- mutate query
  let tokensMaybe = (,) <$> tokenize lang ind <*> tokenize lang mutatedText
  
  case tokensMaybe of
    Nothing -> return (False, query) --error "tokenizer failure"
    Just (tokens, mutatedTokens)  -> do
      searchResult <- performCachedSearch testIndex
                                          lang
                                          (\_ -> return (tokens, ind))
                                          settings
                                          (mutatedText, mutatedTokens)
                                          analyzer
      case searchResult of
        Just rs | numberOfAlignmentMatches rs > 0 -> return (True, mutatedText)
        _ -> return (False, mutatedText)

