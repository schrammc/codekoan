{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Dump.Source.Postgres where

import Data.List (nub)

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dump
import Database.PostgreSQL.Simple

import Data.Text hiding (maximum)
import qualified Data.Vector as V
import Data.Conduit hiding (connect)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad

-- | Create a source of 'StackoverflowPost's from an XML dump file
answerSource :: Connection -> Source (ResourceT IO) Answer
answerSource connection = go 0
  where
    chunkSize = 250 :: Int
    go n = do
      results <- liftIO $ query connection "SELECT * FROM answers WHERE answers.id > ? ORDER BY answers.id LIMIT ?" (n, chunkSize)
      let answers = do
            (aId, aBody, aRating, aParent) <- results
            return $ Answer (AnswerId aId)
                            aBody
                            aRating
                            aParent
          maxId = maximum $ (\(i, _, _, _) -> i)<$> results
      forM_ answers yield
      go maxId

-- | Create a source of 'StackoverflowPost's that have all of a given list of
-- tags
answersWithTags :: Connection -> [Text] -> Source (ResourceT IO) Answer
answersWithTags connection tags = go 0
  where
    chunkSize = 250 :: Int
    tagVector = V.fromList $ nub tags
    go n = do
      results <- liftIO $ query connection "SELECT a.* FROM (SELECT * FROM answers WHERE answers.id > ?) AS a JOIN (SELECT * FROM questions) AS b ON a.parent = b.id WHERE ? <@ tags ORDER BY a.id LIMIT ?" (n, tagVector, chunkSize)
      let answers = do
            (aId, aBody, aRating, aParent) <- results
            return $ Answer (AnswerId aId)
                            aBody
                            aRating
                            aParent
          maxId = maximum $ (\(i, _, _, _) -> i)<$> results
      forM_ answers yield
      go maxId

-- | Get an answer from postgres by id
getAnswer :: Connection -> AnswerId  -> MaybeT IO Answer
getAnswer connection AnswerId{..} = do
  results <- lift $ query connection
                          "SELECT * FROM answers WHERE answers.id = ?"
                          [answerIdInt]
  let answers = do
        (aId, aBody, aRating, aParent) <- results
        return $ Answer (AnswerId aId)
                        aBody
                        aRating
                        aParent
  case answers of
    [] -> MaybeT $ return Nothing
    (a:_) -> return a
  
-- | Get a question from postgres by id
getQuestion :: Connection -> QuestionId  -> MaybeT IO Question
getQuestion connection QuestionId{..} = do
  results <- lift $ query connection
                          "SELECT * FROM questions WHERE questions.id = ?"
                          [questionIdInt]
  let questions = do
        (qId, qTags, qBody, qTitle, qRating) <- results
        return $ Question (QuestionId qId)
                          qBody
                          qTitle
                          qRating
                          qTags
  case questions of
    [] -> MaybeT $ return Nothing
    (q:_) -> return q
  
postgresSOAccess :: Connection -> StackoverflowAccess IO
postgresSOAccess conn =
  StackoverflowAccess { getSOQuestion = getQuestion conn
                      , getSOAnswer   = getAnswer conn
                      }
