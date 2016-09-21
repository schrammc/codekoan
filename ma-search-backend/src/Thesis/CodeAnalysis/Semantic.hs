{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}
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
module Thesis.CodeAnalysis.Semantic where

import Data.Text (Text)
import qualified Data.List as List

import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Range
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Semantic.IdentifierSplitter
import Thesis.Search.AlignmentMatch
import Thesis.Search.ResultSet
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List

import qualified Data.Map as M

import Control.Monad
import Data.Maybe(fromMaybe)

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
resultsWithSimilarity :: MonadThrow m =>
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
                      -> m (Maybe (ResultSet t l))
resultsWithSimilarity lang
                      dict
                      analyzer@SemanticAnalyzer{..}
                      queryDoc
                      set
                      thresh = do
  patternData <- runMaybeT relevantPatterns
  let flattenedSet = flattenSet set
  case patternData of
    Nothing -> return Nothing
    Just patternMap -> do
      flattened' <- runMaybeT $ do
        forM flattenedSet $ \(aId, fragId, matches) -> do
          fragMap <- MaybeT $ return (M.lookup aId patternMap)
          fragData <- MaybeT $ return (M.lookup fragId fragMap)
          sim <- lift $ searchResultSimilarity lang
                                               analyzer
                                               queryDoc
                                               fragData
                                               matches
          if sim > thresh
            then MaybeT . return $ Just (aId, fragId, matches)
            else MaybeT $ return Nothing
      return $ buildSet <$> flattened'
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

  
searchResultSimilarity :: (Monad m) => Language t l
                       -> SemanticAnalyzer m a
                       -> (TokenVector t l, LanguageText l)
                       -- ^ query document
                       -> (TokenVector t l, LanguageText l)
                       -- ^ answer fragment
                       -> [AlignmentMatch t l]
                       -> m Double
searchResultSimilarity lang SemanticAnalyzer{..} (queryTokens, queryText) (fragTokens, fragText) (ms) =
  semanticSimilarity (semanticPreprocess queryIds) (semanticPreprocess fragIds)
  where
    queryIds :: [Text]
    queryIds = identifiers lang queryText $ V.concat $ do
      AlignmentMatch{..} <- ms
      let queryTokenRange = convertRange resultQueryRange
      return $ vectorInRange queryTokenRange queryTokens
    fragIds :: [Text]
    fragIds = identifiers lang fragText $ V.concat $ do
      AlignmentMatch{..} <- ms
      let fragmentTokenRange = convertRange resultFragmentRange
      return $ vectorInRange fragmentTokenRange fragTokens
      

-- | Get the words in all identifiers that underly identifier tokens as split by
-- 'splitIdText'
identifierWordsInCode :: Language t l -> LanguageText l -> Maybe [Text]
identifierWordsInCode lang lt = 
  (flip fmap) (processAndTokenize lang lt) $ \tokenVector -> do
    ident <- identifiers lang lt tokenVector
    splitIdText ident
