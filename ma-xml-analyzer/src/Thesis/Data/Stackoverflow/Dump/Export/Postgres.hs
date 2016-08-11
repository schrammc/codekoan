-- | This module provides functionality to store posts from an XML data dump of
-- Stackoverflow posts in a PostgreSQL database.
--
-- Author: Christof Schramm
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.Data.Stackoverflow.Dump.Export.Postgres (dumpToPostgres) where

import Thesis.Data.Stackoverflow.StackoverflowPost
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dump

import Thesis.Util.ConduitUtils

import Data.Conduit hiding (connect)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

import Database.PostgreSQL.Simple

-- | Writes the information in the Stackoverflow XML dump to a specified
-- postgresql database. Uses
dumpToPostgres :: FilePath -> ConnectInfo -> IO ()
dumpToPostgres dumpPath connInfo = do
  connection <- connect connInfo
  
  makePostgresSchema connection

  let chunkSize = 500

  runResourceT $ do
    postSource dumpPath $$
      chunks chunkSize
      =$= everyN (25000 `div` chunkSize) (\k -> liftIO $ print $ chunkSize * k)
      =$= awaitForever return

  return ()

-- | Split the conduit into chunks of length n.
--
-- If the conduit has less than n values left then the last chunk will have a
-- size less than n.
chunks :: Monad m =>
          Int -- ^ Chunk size values < 1 will be set to 1
          -> ConduitM a [a] m ()
chunks n = do
  go [] (max 1 n) >>= yield
  chunks n
  where
    go xs k | k == 0 = return xs
            | otherwise = do
              nextMaybe <- await
              case nextMaybe of
                Just next -> go (next:xs) (k-1)
                Nothing   -> return xs
      
-- | Create a schema for the dump data in Postgresql
makePostgresSchema :: Connection -> IO ()
makePostgresSchema conn = do
  execute_ conn "DROP TABLE IF EXISTS answers"
  execute_ conn "DROP TABLE IF EXISTS questions"

  execute_ conn "CREATE TABLE answers(id INTEGER NOT NULL, body TEXT, rating INTEGER, parent INTEGER NOT NULL, PRIMARY KEY(id))"
  execute_ conn "CREATE TABLE questions(id INTEGER NOT NULL, tags TEXT[], body TEXT, title TEXT, rating INTEGER, PRIMARY KEY(id))"
  return ()

-- | Write a set of posts to the given database
writePosts :: Connection -> [StackoverflowPost] -> IO ()
writePosts conn posts = do
  writeAnswers conn answers
  writeQuestions conn questions
  return ()
  where
    (answers, questions) = foldl (\(as,qs) -> \post -> case post of
                                     PostAnswer a   -> (a:as, qs  )
                                     PostQuestion q -> (as  , q:qs)
                                     _ -> (as,qs)
                                 ) ([],[]) posts

-- | Write a set of answers to the "answers" table of the given database
writeAnswers :: Connection -> [Answer] -> IO ()
writeAnswers connection as = do
  let transformed = do
        Answer{..} <- as
        return $ ( answerIdInt answerId
                 , answerBody
                 , answerRating
                 , answerParent
                 )
  executeMany connection "INSERT INTO answers VALUES (?,?,?,?)" transformed
 
  return ()

-- | Write a set of questions to the "questions" table of the given database
writeQuestions :: Connection -> [Question] -> IO ()
writeQuestions connection qs = do
  let transformed = do
        Question{..} <- qs
        return ( questionIdInt questionId
               , questionTags
               , questionBody
               , questionTitle
               , questionRating
               )
  executeMany connection "INSERT INTO questions VALUES (?,?,?,?,?)" transformed

  return ()
