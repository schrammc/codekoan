-- |
-- Description: Clustering of patterns
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
module Thesis.PatternClustering where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import           Data.Algorithm.MaximalCliques
import qualified Data.Conduit.List as CL
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.Hashable (Hashable)
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
                   , Eq ann, Hashable ann, FragmentData ann
                   , MonadIO m, MonadLogger m, MonadThrow m
                   , Foldable f) =>
                   Language t l
                -> (FragmentId ann -> Int -> ann)
                -> SemanticAnalyzer m a
                -> SearchSettings
                -> f (FragmentId ann, LanguageText l)
                -> m [[FragmentId ann]]
clusterPatterns lang toFragData semanticAnalyzer settings patterns = do
  index <- buildIndex lang source 10
  links <- forM (toList patterns) $ \(ann, txt) -> do
    resultSetMaybe  <- performSearch index
                                     lang
                                     lookupPattern
                                     settings
                                     txt
                                     semanticAnalyzer
    case resultSetMaybe of
      Nothing -> return (ann, [])
      Just resultSet -> return (ann, fmap getFragmentId $
                                       M.keys $ resultSetMap resultSet)
  let linkMap = M.fromList $ (\(a,ls) -> (a, S.fromList ls)) <$> links
  return $ getMaximalCliques (doublyLinked linkMap) (M.keys linkMap)
  where
    patternMap = foldl (\mp (ann, txt) ->
                           let tks = processAndTokenize lang txt
                               i = toFragData ann (length tks)
                           in M.insert i (tks, txt) mp)
                       M.empty
                       patterns
    lookupPattern ann = case M.lookup ann patternMap of
                          Just (Just tks, txt) -> return (tks, txt)
                          _ -> MaybeT $ return Nothing
    source = CL.sourceList $ (\(ann, t) -> (toFragData ann, t)) <$> toList patterns
    doublyLinked linkMap a b = linked linkMap a b && linked linkMap b a
    linked linkMap a b = case M.lookup a linkMap of
      Just s -> S.member b s
      Nothing -> error "Impossible case (pattern clustering)"
    
