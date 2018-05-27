-- |
-- Description: Clustering of patterns
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Thesis.PatternClustering where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import qualified Data.Conduit.List as CL
import           Data.Foldable (foldl')
import           Data.Foldable (toList)
import           Data.Graph
import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable)
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Semantic.Internal
import           Thesis.Search
import           Thesis.Search.FragmentData
import           Thesis.Search.Index
import           Thesis.Search.ResultSet
import           Thesis.Search.Settings

-- | Build clusters of a set of patterns. These clusters are cliques in a graph
-- of fragment ids. Two fragments in this graph are connected if each is similar
-- to the other with the given search settings.
--
-- This function constructs an index from the given patterns in the given
-- language. Then each pattern is run against the index and the results
-- collected to build the graph mentioned above.
clusterPatterns :: ( Eq t, Hashable t, NFData t
                   , Eq ann, Hashable ann, FragmentData ann, Ord (FragmentId ann)
                   , MonadIO m, MonadLogger m, MonadThrow m
                   , Foldable f) =>
                   Language t l
                -> (FragmentId ann -> Int -> ann)
                -> SemanticAnalyzer m a
                -> SearchSettings
                -> f (FragmentId ann, LanguageText l)
                -> m [[FragmentId ann]]
clusterPatterns lang toFragData semanticAnalyzer settings patterns = do
  index <- buildIndex lang source (minMatchLength settings)
  links <- forM (toList patterns) $ \(ann, txt) -> do
    let Just (tks, _) = M.lookup ann patternMap
    resultSetMaybe  <- performSearch index
                                     lang
                                     lookupPattern
                                     settings
                                     (txt, tks)
                                     semanticAnalyzer
    case resultSetMaybe of
      Nothing -> return (ann, ann, [])
      Just resultSet -> {-# SCC theculprit #-}
        let results =  fmap getFragmentId $
                       M.keys $ resultSetMap resultSet
        in results `seql` return (ann, ann, results)
  return $ flattenSCC <$> stronglyConnComp links
  where
    patternMap = foldl' (\mp (ann, !txt) ->
                           case processAndTokenize lang txt of
                             Nothing -> mp
                             Just !tks -> M.insert ann (tks, txt) mp)
                       M.empty
                       patterns
    lookupPattern ann = case M.lookup (getFragmentId ann) patternMap of
                          Just (tks, txt) -> return (tks, txt)
                          _ -> MaybeT $ return Nothing
    source = CL.sourceList $ (\(ann, t) -> (toFragData ann, t)) <$> toList patterns
    seql [] v = v
    seql (x:xs) v = seq x (seql xs v)

clusterAsPatterns :: ( Eq t, Hashable t, NFData t
                     , MonadIO m, MonadLogger m, MonadThrow m) =>
                     Language t l
                  -> SemanticAnalyzer m a
                  -> SearchSettings
                  -> [(x,LanguageText l)]
                  -> m [[x]]
clusterAsPatterns l analyzer settings patterns = do
  indices <- clusterPatterns l toFragData analyzer settings (zip [0..] texts)
  return $ fmap (fmap (V.unsafeIndex annVector)) indices
  where
    toFragData :: Int -> Int -> (Int, Int)
    toFragData n = \len -> (n, len)
    (annotations, texts) = unzip patterns
    annVector = V.fromList annotations
