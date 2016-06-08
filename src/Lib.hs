{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import           Data.Binary

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource

import           Control.Concurrent.MVar

import           Data.Attoparsec.Text as AP

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import           Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import           Data.List (foldl', sortOn)

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import           Data.Vector (Vector)
import           Data.Vector.Binary
import qualified Data.Vector as V

import GHC.Generics (Generic)

import           Data.XML.Types (Event)
import           Text.XML.Stream.Parse

import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.Tokenizer
import           Thesis.Trie as Trie
import           Thesis.Levenstein
import Thesis.Data.Stackoverflow.StackoverflowPost
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dictionary
  

import System.IO

import Data.Monoid


xmlFilePath :: String
xmlFilePath = "/home/kryo/posts_abridged.xml"
--xmlFilePath = "/home/kryo/data/stackoverflow_dump/xmlfiles/Posts.xml"

outfile = "/home/kryo/out"

questionStatfile = "/home/kryo/question_stats"
answerStatfile = "/home/kryo/answer_stats"

writePost (PostQuestion Question{..}) =
  mconcat [ "{\"type\":\"question\""
          , ",\"id\":" <> (Text.pack $ show $ questionIdInt questionId)
          , ",\"code\":" <> (Text.pack . show $ readCodeFromHTMLPost questionBody)
          , ",\"tags\":" <> (Text.pack $ show questionTags)
          , "}"
          ]
writePost (PostAnswer Answer{..}) =
    mconcat [ "{\"type\":\"answer\""
            , ",\"id\":" <> (Text.pack $ show $ answerIdInt answerId)
            , ",\"code\":" <> (Text.pack . show $ readCodeFromHTMLPost answerBody)
            , "}"
            ]

buildTrieForAnswersWithTag :: Text -> IO (Trie Token AnswerId)
buildTrieForAnswersWithTag tag = do
  content <- BS.readFile "/home/kryo/data/stackoverflow_dump/data_dictionary"
  let dict = decode (BL.fromChunks [content]):: DataDictionary
  dict `seq` print "Done with dictionary"
  nVar <- newMVar (0 :: Integer)
  
  runResourceT $ do
    postSource
      $$ (CL.iterM $ \_ -> lift $ do -- Print a message every 25000 posts to
                                     -- show progress
              n <- takeMVar nVar
              putMVar nVar $! (n+1)
              when (n `mod` 25000 == 0) $ putStrLn $ show n
          )
      =$= filterAnswers
      =$= (CL.filter $ \Answer{..} -> answerWithTag dict tag answerId)
      =$= (CL.map $ \Answer{..} -> (,answerId) <$> readCodeFromHTMLPost answerBody)
      =$= CL.concat
      =$= (CL.map $ \(c, aId) -> (,aId) <$> processAndTokenize java c)
      =$= CL.catMaybes
      =$= CL.map (uncurry linearTrie)
      =$ (CL.fold mergeTries Trie.empty)

someFunc :: IO ()
someFunc = do
  trie <- buildTrieForAnswersWithTag (Text.pack "java")
  
  putStrLn "Trie is built"
  go trie
  where
    go trie = do
      putStrLn "Enter query:"
      str <- getLine
      putStrLn "Sensitivity: "
      n <- read <$> getLine
      let tks = fromJust $ processAndTokenize java (Text.pack str)
          aut = LevensteinAutomaton (length tks)
                                    n
                                    (tks V.!)
          res = sortOn (\(_,_,similarity) -> similarity) $  lookupL aut trie
      putStrLn "Result: "
      print $ (\(_,id, similarity) -> (id, similarity)) <$> res
      putStrLn ""
      go trie
  


postSource :: Source (ResourceT IO) StackoverflowPost
postSource = parseFile def xmlFilePath =$= parsePosts

tagValue :: MVar (M.Map Text Int, Int) -> Text -> IO Int
tagValue mpVar t = do
  (mp, n) <- takeMVar mpVar
  case M.lookup t mp of
    Just x  -> putMVar mpVar (mp, n) >> return x
    Nothing -> putMVar mpVar (M.insert t n mp, n+1) >> return n

writeQuestionTags :: (MonadTrans t, Monad (t IO))
                     => MVar (M.Map Text Int, Int)
                     -> MVar (IntMap (S.Set Int))
                     -> Conduit StackoverflowPost (t IO) StackoverflowPost
writeQuestionTags tagV v = CL.iterM $ \p ->
  case p of
    (PostQuestion Question{..}) -> lift $ do
      qInts <- V.mapM (tagValue tagV) questionTags
      let qIntsSet = S.fromList $ V.toList qInts
      modifyMVar_ v $ \mp ->
        return $! IM.insert (questionIdInt questionId) qIntsSet mp
    _ -> return ()
writeAnswerRoots :: (MonadTrans t, Monad (t IO))
                    => MVar (IntMap Int)
                    -> Conduit StackoverflowPost (t IO) StackoverflowPost
writeAnswerRoots v = CL.iterM $ \p ->
  case p of
    (PostAnswer !Answer{..}) -> lift $ modifyMVar_ v $ \mp ->
      return $! IM.insert (answerIdInt answerId) answerParent mp
    _ -> return ()

filterTags :: Monad m => S.Set Text -> Conduit Question m Question
filterTags tgs = CL.filter $ \Question{..} ->
                               not . null $ filter (\t -> S.member t tgs)
                                                   (V.toList questionTags)
