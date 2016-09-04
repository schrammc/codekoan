{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Data.Stackoverflow.Dictionary.Database where

import Data.Text hiding (concat, replicate, length, intersperse)

import Data.List (intersperse)

import           Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Set as S

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Dump
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.StackoverflowPost

import System.Directory

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
         
-- | Get the text tags for an answer's parent question
answerRootTags :: (IConnection c) => c -> AnswerId -> IO (Maybe (S.Set Text))
answerRootTags conn aId = do
  maybeQid <- getAnswerParent conn aId
  case maybeQid of
    Nothing -> return Nothing
    Just QuestionId{..} -> do
      tagInts <-
        concat <$> quickQuery' conn
                               "SELECT tag FROM questionTags WHERE id = ?"
                               [toSql questionIdInt]

      tagList <-
        concat <$> quickQuery' conn
                   ("SELECT tagtext FROM tags WHERE id in " ++
                    buildParamList tagInts)
                   (toSql <$> tagInts)

      return $ Just (S.fromList $ fromSql <$> tagList)
  where
    buildParamList lst = 
      "(" ++ (intersperse ',' $ replicate (length lst) '?') ++ ")"

-- | Get the text tags for an answer's parent question
getAnswerParent :: (IConnection c) => c -> AnswerId -> IO (Maybe QuestionId)
getAnswerParent conn AnswerId{..} = do
  xs <- concat <$> quickQuery' conn
                               "SELECT parent FROM answers WHERE answers.id = ?"
                               [toSql answerIdInt]
  case xs of
    [] -> return Nothing
    (x:_) -> return $ Just (QuestionId $ fromSql x)
  

-- | See if the answer's parent question is tagged with the given tag
answerWithTag :: (IConnection c) => c -> Text -> AnswerId -> IO Bool
answerWithTag conn tag aId = do
  tags <- answerRootTags conn aId
  case tags of
    Just ts -> return $ S.member tag ts
    Nothing -> return False

-- | Build a dictionary structure in an Sqlite databse. The dictionary structure
-- will help us answer questions like:
--   * What is the parent question of an answer
--   * What tags does the parent question of an answer have
--   * What other answers exist to a question besides the given one
buildSqliteDict :: FilePath -> FilePath -> IO ()
buildSqliteDict dbPath dataPath = do
  
  -- Clear the DB if it exists already
  dbFileExists <- doesFileExist dbPath
  when dbFileExists $ removeFile dbPath

  conn <- connectSqlite3 dbPath
  createSqlSchema conn
  counter <- newMVar (0 :: Int)
  runResourceT $ do
    postSource dataPath $$
      (CL.iterM $ \post -> case post of
          PostUnknown -> return ()
          PostAnswer Answer{..} -> lift $ do

            run conn "INSERT INTO answers VALUES (?, ?)"
                      [toSql $ answerIdInt answerId, toSql answerParent]
            commit conn
            return ()
          PostQuestion Question{..} -> lift $ do
            qtags <- forM questionTags $ \t -> do
              tIds <-
                concat <$> quickQuery' conn
                                      "SELECT id FROM tags WHERE tags.tagtext = ?"
                                      [toSql t]
              case tIds of
                [] -> do
                  modifyMVar_ counter (return . (+1))
                  c <- readMVar counter
                  run conn "INSERT INTO tags VALUES (?, ?)" [toSql c, toSql t]
                  return c
                (x:[]) -> return $ fromSql x
                (_:_:_) -> error "Tag has double string"

            forM_ qtags $ \t -> do
              run conn "INSERT INTO questionTags VALUES (?, ?)"
                        [toSql $ questionIdInt questionId, toSql t] 
            run conn "INSERT INTO questions VALUES (?)"
                      [toSql $ questionIdInt questionId]
            
            commit conn
            return ()
          

      )
      =$ awaitForever return
        
  disconnect conn

createSqlSchema :: IConnection c => c -> IO ()
createSqlSchema conn = do
  run conn "CREATE TABLE answers (id INTEGER NOT NULL, parent INTEGER NOT NULL, PRIMARY KEY (id))" []
  run conn "CREATE TABLE questions (id INTEGER NOT NULL, PRIMARY KEY (id))" []
  run conn "CREATE TABLE tags (id INTEGER NOT NULL, tagtext VARCHAR(80), PRIMARY KEY (id, tagtext))" []
  run conn "CREATE TABLE questionTags (id INTEGER NOT NULL, tag INTEGER NOT NULL, PRIMARY KEY(id, tag))" []
  return ()
