-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- THIS IS AN INTERNAL MODULE, DO NOT USE EXCEPT FOR IN UNIT TESTS, ETC.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.CodeAnalysis.Semantic.Internal where


import           Data.Text (Text)
import qualified Data.Text as Text


import           Control.Monad.Logger

import           Thesis.Data.Range
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic.IdentifierSplitter
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.ResultSet
import           Thesis.SearchException
import           Control.Monad.Trans.Class
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Thesis.Util.MonadUtils

import           Data.Maybe

import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable)

import           Control.Monad
import           Debug.Trace


import qualified Data.Vector as V

-- | An interface to a semantic analysis method
data SemanticAnalyzer m a =
  SemanticAnalyzer { semanticPreprocess :: [Text] -> a
                   , semanticSimilarity :: a -> a -> m Double
                     -- ^ This function must return double values on a scale of
                     -- 0.0 to 1.0. 0.0 is maximally dissimilar while 1.0 is
                     -- either identical or highly similar.
                   }

-- | This function filters all those results from a result set that don't have a
-- semantic similarity value that is equal to or greater than the reference
-- value.
--
-- This will return 'Nothing' if a fragment can't be found in the dictionary
resultsWithSimilarity :: (MonadLogger m, MonadThrow m
                         , Hashable ann, Eq ann) =>
                         Language t l
                         -- ^ We always work in relation to a given language
                         -- with fixed tokens
                      -> (ann -> MaybeT m (TokenVector t l, LanguageText l))
                      -- ^ An accessor for code pattern data
                      -> SemanticAnalyzer m a
                      -> (TokenVector t l, LanguageText l)
                      -- ^ query document
                      -> ResultSet t l ann
                      -- ^ The result set to be analyzed
                      -> Double
                      -- ^ Threshold value between 0 and 1
                      -> MaybeT m (ResultSet t l ann)
resultsWithSimilarity lang
                      getTokenV
                      analyzer@SemanticAnalyzer{..}
                      queryDoc
                      set
                      thresh = do
  -- TODO: Optimization use mapMaybeWithKey here.
  let resultSetList  = M.toList $ resultSetMap set
  lst <- catMaybeTs $ (flip fmap) resultSetList $ \(ann, groups) -> do
    tv <- getTokenV ann
    let groupResults = (flip fmap) groups $ \group -> do
          sim <- lift $ searchResultSimilarity lang analyzer queryDoc tv group
          if sim >= thresh
            then return group
            else MaybeT $ return Nothing
    filteredGroups <- catMaybeTs groupResults
    if null filteredGroups
      then MaybeT $ return Nothing
      else return (ann, filteredGroups)

  return $ ResultSet (M.fromList lst)

searchResultSimilarity :: (MonadLogger m, MonadThrow m) =>
                          Language t l
                       -> SemanticAnalyzer m a
                       -> (TokenVector t l, LanguageText l)
                       -- ^ query document
                       -> (TokenVector t l, LanguageText l)
                       -- ^ answer fragment
                       -> [AlignmentMatch t l ann]
                       -> m Double
searchResultSimilarity lang
                       SemanticAnalyzer{..}
                       (queryTokens, queryText)
                       (fragTokens, fragText)
                       matches = do
  fragIds <- getIds lang (fragTokens, fragText) matches Fragment
  queryIds <- getIds lang (queryTokens, queryText) matches Query
  semanticSimilarity (semanticPreprocess queryIds) (semanticPreprocess fragIds)

data QueryOrFragment = Query | Fragment
                     deriving (Eq)

-- | A helper function, that builds a list of the words of all identifiers in
-- the given alignment matches, depending on wether we are dealing with query or
-- fragment data.
getIds :: (MonadLogger m, MonadThrow m) =>
          Language t l
       -> (TokenVector t l, LanguageText l)
       -> [AlignmentMatch t l ann]
       -> QueryOrFragment
       -> m [Text]
getIds lang (tokens, text) matches queryOrFragment = do
      v <- forM matches $ \match -> do
        let range = tokenRange match
        case vectorInRange range tokens of
          Just vector -> return vector
          Nothing     -> do
            $(logError) $ (Text.pack $ buildErrorMsg match)
            throwM $ (SemanticException $ buildErrorMsg match)
      let listOfIds = identifiers lang text (V.concat v)
      return . concat $ (splitIdText <$> listOfIds)
  where
    tokenRange match = convertRange $ 
      if queryOrFragment == Fragment
      then resultFragmentRange match
      else resultQueryRange match
    buildErrorMsg match  =
      if queryOrFragment == Fragment
      then "Failure: slice (fragmentTokenRange)"  ++ (show $ tokenRange match) ++
           " -- " ++ (show $ V.length tokens)
      else "Failure: slice (queryTokenRange)"  ++ (show $ tokenRange match) ++
           " -- " ++ (show $ V.length tokens)

-- | Get the words in all identifiers that underly identifier tokens as split by
-- 'splitIdText'
identifierWordsInCode :: Language t l -> LanguageText l -> Maybe [Text]
identifierWordsInCode lang lt = 
  (flip fmap) (processAndTokenize lang lt) $ \tokenVector -> do
    ident <- identifiers lang lt tokenVector
    splitIdText ident
