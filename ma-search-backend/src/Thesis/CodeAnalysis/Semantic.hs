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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.CodeAnalysis.Semantic
       ( SemanticAnalyzer(..)
       , resultsWithSimilarity
       , identifierWordsInCode
       )where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import Control.Monad.Logger
import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Data.Range
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Semantic.IdentifierSplitter
import Thesis.Search.AlignmentMatch
import Thesis.Search.ResultSet
import Thesis.SearchException
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Trans.Maybe

import Data.Maybe

import qualified Data.Map as M

import Control.Monad

import Data.Vector ((!?))
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
resultsWithSimilarity :: (MonadLogger m, MonadThrow m) =>
                         Language t l
                         -- ^ We always work in relation to a given language
                         -- with fixed tokens
                      -> DataDictionary m
                      -- ^ An accessor for code pattern data
                      -> SemanticAnalyzer m a
                      -> (TokenVector t l, LanguageText l)
                      -- ^ query document
                      -> ResultSet t l
                      -- ^ The result set to be analyzed
                      -> Double
                      -- ^ Threshold value between 0 and 1
                      -> MaybeT m (ResultSet t l)
resultsWithSimilarity lang
                      dict
                      analyzer@SemanticAnalyzer{..}
                      queryDoc
                      set
                      thresh = do
  patternMap <- relevantPatterns
  let flattenedSet = flattenSet set
  flattened' <- do
    catMaybeTs $ (flip fmap) flattenedSet $ \(aId, fragId, matches) -> do
      fragMap <- MaybeT $ return (M.lookup aId patternMap)
      fragData <- MaybeT $ return (M.lookup fragId fragMap)
      sim <- lift $ searchResultSimilarity lang
                                           analyzer
                                           queryDoc
                                           fragData
                                           matches
      if sim >= thresh
        then MaybeT . return $ Just (aId, fragId, matches)
        else MaybeT $ return Nothing
  return $ buildSet flattened'
  where
    -- | Builds up a map with data on all fragments in the result set.
    relevantPatterns = fmap M.fromList $ forM fragmentMap $ \(aId, frags) -> do
      fragments <- answerFragments dict lang aId
      fragVectors <- sequence $ do
        fragId <- frags
        return $ MaybeT $ return $ (fragId,) <$> fragments !? fragId
      return $ (aId, M.fromList $ fragVectors)
      where
        relevantFragments = List.nub $ do
          (aId, fragId, _) <- flattenSet set
          return $ (aId, fragId)
        fragmentMap = do
          group <- List.groupBy (\a -> \b -> fst a == fst b) relevantFragments
          let (aId, _) = head group
          return (aId, snd <$> group)

catMaybeTs :: (Functor f, Monad f) => [MaybeT f a] -> MaybeT f [a]
catMaybeTs xs = MaybeT $ Just <$> maybeList

  where
    maybeList = catMaybes <$> (mapM runMaybeT xs)

  
searchResultSimilarity :: (MonadLogger m, MonadThrow m) => Language t l
                       -> SemanticAnalyzer m a
                       -> (TokenVector t l, LanguageText l)
                       -- ^ query document
                       -> (TokenVector t l, LanguageText l)
                       -- ^ answer fragment
                       -> [AlignmentMatch t l]
                       -> m Double
searchResultSimilarity lang SemanticAnalyzer{..} (queryTokens, queryText) (fragTokens, fragText) matches = do
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
       -> [AlignmentMatch t l]
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
      return $ identifiers lang text (V.concat v)
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
