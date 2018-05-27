{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import Thesis.CodeAnalysis.Language.Java
-- import Thesis.CodeAnalysis.Semantic
import Thesis.CodeAnalysis.Semantic.Chatter
import NLP.Types

someFunc :: IO ()
someFunc = do
  corpus <- chatterDirectoryCorpus java "/Users/kryo/temp/storm/"
  putStrLn $ show $ corpus
  {-
  dict <- readDictionary dictPath
  putStrLn "The dictionary is complete"
  index <- dict `seq` buildIndexForJava dict xmlFilePath 10
  putStrLn "Index construction completed."
  go index
  where
    go :: (Hashable t, Ord t, Show t) => SearchIndex t l -> IO ()
    go index = do
      putStrLn "Enter query: "
      str <- Text.pack <$> getLine
      print str
      n <- readPrompt "Sensitivity: "
      let res = sortOn resultLevenScore <$> (listOfResults <$> findMatches index n (LanguageText str))
      putStrLn "Result: "
      case res of
        Just results@(_:_) -> mapM_ (printOutput str)
                                    (filter (\SearchResult{..} -> length resultMatchedTokens > 10) (results))
        _ -> putStrLn "Not a single thing. Sorry."
      putStrLn "===================================================\n"
      go index

printOutput :: Show t
               => Text
            -> SearchResult t
            -> IO ()
printOutput t res = do
  putStrLn "Found a match: "
  putStrLn $ prettyPrintOutput t res

prettyPrintOutput :: (Show t)
                     => Text
                  -> SearchResult t
                  -> String
prettyPrintOutput t SearchResult{..} = do
  let res = Text.unpack $ textInRange resultTextRange t
  "Distance: " ++ show resultLevenScore
              ++ " In answer fragment: " ++ show (fragmentMetaId resultMetaData)
              ++ " In range: " ++  show resultTextRange
              ++ " Matched tokens: " ++ show resultMatchedTokens ++ "\n"
              ++ " Source:\n" ++ res

-- | Try to read something and reprompt if the user puts in something that we
-- can't understand until we have a valid result
readPrompt :: (Read a) => String -> IO a
readPrompt str = do
  putStrLn str
  catch readLn $ \(_ :: IOException) -> do
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
-}
