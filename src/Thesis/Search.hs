{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search where

import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.ST
import Data.Hashable (Hashable,hash)

import qualified Data.BloomFilter as BF
import qualified Data.BloomFilter.Easy as BF.Easy
import qualified Data.BloomFilter.Hash as BF.Hash
import qualified Data.BloomFilter.Mutable as BF.Mutable

import Data.Binary

import Control.Concurrent.MVar
import Control.Monad.Trans.Resource
import Thesis.BloomFilter
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Data.Stackoverflow.Dump
import Thesis.Data.Stackoverflow.StackoverflowPost
import Thesis.CodeAnalysis.StackoverflowBodyParser
import Thesis.Levenstein
import Thesis.NGrams
import Thesis.Tokenizer
import Thesis.CompressedTrie as Trie
import Thesis.Data.Text.PositionRange

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector as V
import Debug.Trace

data SearchIndex t l where
  SearchIndex :: (Ord t, Eq t) => { indexLanguage :: !(Language t l)
                 , indexTrie :: !(CompressedTrie t AnswerId)
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l

buildIndexForJava :: DataDictionary -- ^ The data dictionary
                  -> FilePath -- ^ Path to the file with the posts
                  -> Int      -- ^ NGram size
                  -> IO (SearchIndex Token Java)
buildIndexForJava dict postsFile ngramSize = do
  let (bloomSize, bloomNumberFs) = BF.Easy.suggestSizing 1000000 0.01
      hashF = BF.Hash.hashes bloomNumberFs  

  mutableBF <- stToIO $ BF.Mutable.new hashF bloomSize
  
  nVar <- newMVar (0 :: Integer)
  
  tr <- runResourceT $ do
    postSource postsFile
      $$ (CL.iterM $ \_ -> lift $ do -- Print a message every 25000 posts to
                                     -- show progress
              n <- takeMVar nVar
              putMVar nVar $! (n+1)
              when (n `mod` 25000 == 0) $ putStrLn $ show n
          )
      =$= filterAnswers
      =$= (CL.filter $ \Answer{..} -> answerWithTag dict "java" answerId)
      =$= (CL.map $ \Answer{..} -> (,answerId) <$> readCodeFromHTMLPost answerBody)
      =$= CL.concat
      =$= (CL.map $ \(c, aId) -> (,aId) <$> buildTokenVector java (LanguageText c))
      =$= CL.catMaybes
      =$ (CL.foldM (\trie -> \(str, v) -> do
                       mapM_ (lift . stToIO . BF.Mutable.insert mutableBF) $ do
                         (hash <$> ngrams ngramSize (V.toList str))
                       return $ mergeTries trie (linearTrie str v)
                   )
                   Trie.empty
         )

  bf <- BloomFilter <$> (stToIO $ BF.freeze mutableBF)
  return $ SearchIndex java tr bf ngramSize

-- | Build a search index from data stored in binary files
loadIndex :: (Hashable t, Binary t, Ord t) => Language t l
          -> FilePath -- ^ The index path
          -> Int      -- ^ NGram size
          -> IO (SearchIndex t l)
loadIndex lang indexPath ngramSize = do
  trie <- decodeFile indexPath
  return $ SearchIndex lang trie (buildTrieBloom ngramSize trie) ngramSize

-- | Build a bloom filter containing all 'n' - long nonoverllapping ngrams of
-- words in the given tire
buildTrieBloom :: (Hashable t)
                  => Int      -- ^ Length of the ngrams that are stored in the
                              -- bloomfilter
                  -> CompressedTrie t a -- ^ The trie whose word's ngrams we
                                        -- want to store in the bloom filter
                  -> BloomFilter [t]
buildTrieBloom n trie = buildBloomList $ do
  (str, _) <- wordsInTrie' trie
  ngram <- allNGrams n str
  return ngram

findMatches :: (Ord t, Hashable t)
               => SearchIndex t l 
               -> Int             -- ^ The tolerated levenstein distance
               -> LanguageText l  -- ^ The submitted code to be searched
               -> Maybe [(PositionRange, [t], AnswerId, Int)]
findMatches index@(SearchIndex{..}) n t = do
  tokens <- maybeTokens
  let ngramsWithTails = allNgramTails indexNGramSize tokens
      relevantNGramTails = let before = length ngramsWithTails
                               result = filter (ngramRelevant . fst) ngramsWithTails
                               after = length result
                           in traceShow (fromIntegral after / (fromIntegral before :: Double))  result
      relevantTails = snd <$> relevantNGramTails
  return (concat $ searchFor <$> relevantTails)
  where
    maybeTokens = processAndTokenize indexLanguage t
    ngramRelevant tks = indexBF =?: (snd <$> tks)

    searchFor ts  = 
      let tokenVector = V.fromList $ snd <$> ts
          result = search index n tokenVector
      in do
           (foundTokens, aId, score) <- result
           case ngramWithRange (take (length foundTokens) ts) of
             Nothing -> []
             Just x -> return (fst x, foundTokens, aId, score)

-- | A range starting at the start of the first range and ending at the end of
-- the second range
mergePositionRanges :: PositionRange -> PositionRange -> PositionRange
mergePositionRanges (PositionRange start _) (PositionRange _ end) =
                        PositionRange start end

-- | For an ngram with (assumed) contiguous tokens give us the position range of
-- the whole ngram and the ngram
ngramWithRange :: [(PositionRange, t)] -> Maybe (PositionRange, [t])
ngramWithRange [] = Nothing
ngramWithRange xs = let (rs, ts) = unzip xs
                    in Just (foldl1 mergePositionRanges rs, ts)

search :: (Ord t) => SearchIndex t l -> Int -> V.Vector t -> [([t],AnswerId, Int)]
search SearchIndex{..} n xs =
  lookupAllL aut indexTrie
  where
    aut = LevensteinAutomaton (V.length xs) n (xs V.!)

