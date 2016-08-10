{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Dump.Source.Postgres where

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Database.PostgreSQL.Simple

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
    chunkSize = 250
    go n = do
      results <- liftIO $ query connection "SELECT * FROM answers WHERE answers.id > ? LIMIT ?" [n, chunkSize]
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
  
  
