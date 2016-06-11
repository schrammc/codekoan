{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Search where

import Control.Monad.Trans.Class
import Control.Monad
import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary (decode)

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
import Thesis.Trie as Trie
import Thesis.Data.Text.PositionRange

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Vector as V

data SearchIndex t l where
  SearchIndex :: { indexLanguage :: !(Language t l)
                 , indexTrie :: !(Trie t AnswerId)
                 , indexBF :: !(BloomFilter [t])
                 , indexNGramSize :: !Int
                 } -> SearchIndex t l

buildIndexForJava :: FilePath -- ^ Path to the data dictionary
                  -> FilePath -- ^ Path to the file with the posts
                  -> Int      -- ^ NGram size
                  -> IO (SearchIndex Token Java)
buildIndexForJava dictPath postsFile ngramSize = do
  content <- BS.readFile dictPath
  let dict = decode (BL.fromChunks [content]):: DataDictionary
      ngramLength = 5
  dict `seq` print ("Decoded dictionary" :: String)
  nVar <- newMVar (0 :: Integer)
  
  (tr, bf) <- runResourceT $ do
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
      =$ (CL.fold (\(trie, blf) -> \(str, v) ->
                    let tr = linearTrie str v
                        blf' = blf `bloomMerge` ngramBloom ngramLength str
                    in (mergeTries trie tr, blf')
                  )
                  (Trie.empty, emptyBloom))

  return $ SearchIndex java tr bf ngramSize

findMatches :: (Ord t, Hashable t)
               => SearchIndex t l 
               -> Int             -- ^ The tolerated levenstein distance
               -> LanguageText l  -- ^ The submitted code to be searched
               -> Maybe [(PositionRange, AnswerId, Int)]
findMatches index@(SearchIndex{..}) n t = do
  tokens <- maybeTokens
  let ngramsWithTails = ngramTails indexNGramSize tokens
      relevantNGramTails = filter (ngramRelevant . fst) ngramsWithTails
      relevantTails = snd <$> relevantNGramTails
  return (concat $ searchFor <$> relevantTails)
  where
    maybeTokens = processAndTokenize indexLanguage t
    ngramRelevant tks = indexBF =?: (snd <$> tks)

    searchFor ts  = 
      let tokenVector = V.fromList $ snd <$> ts
          result = search index n tokenVector
      in case ngramWithRange ts of
        Just (range, _) ->
              (\(_, aId, score) -> (range, aId, score)) <$> result
        Nothing -> []

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

