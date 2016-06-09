{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import           Control.Monad.Trans.Class
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Thesis.Tokenizer
import           Thesis.Levenstein
import Thesis.Data.Stackoverflow.StackoverflowPost
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Search

dictPath :: String
dictPath = "/home/kryo/data/stackoverflow_dump/data_dictionary"

xmlFilePath :: String
xmlFilePath = "/home/kryo/posts_abridged.xml"
--xmlFilePath = "/home/kryo/data/stackoverflow_dump/xmlfiles/Posts.xml"

someFunc :: IO ()
someFunc = do
  index <- buildIndexForJava dictPath xmlFilePath 10
  
  putStrLn "Index construction completed."
  go (indexTrie index)
  where
    go trie = do
      putStrLn "Enter query: "
      str <- Text.pack <$> getLine
      print str
      n <- readPrompt "Sensitivity: "
      let tks = fromJust $ buildTokenVector java (LanguageText str)
          aut = LevensteinAutomaton (length tks)
                                    n
                                    (tks V.!)
          res = sortOn (\(_,_,similarity) -> similarity) $  lookupL aut trie
      putStrLn "Result: "
      print res
      putStrLn ""
      go trie

-- | Try to read something and reprompt if the user puts in something that we
-- can't understand until we have a valid result
readPrompt :: (Read a) => String -> IO a
readPrompt str = do
  putStrLn str
  catch readLn $ \(e :: IOException) -> do
                                          putStrLn "Sorry, what?\n"
                                          readPrompt str

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
