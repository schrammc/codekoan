-- | A 'DataDictionary' implementation using a PostgresSQL backend
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Dictionary.Postgres where

import Data.Text (Text)
import Data.Vector as V (toList)

import Database.PostgreSQL.Simple

import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import qualified Data.Set as S (fromList, Set)

-- | A 'DataDictionary' that accesses a PostgresSQL database.
postgresDictionary :: Connection -> IO (DataDictionary IO)
postgresDictionary connection =
  return $ DataDictionary{ answerParent = postgresAnswerParent connection
                         , questionTags = postgresQuestionTags connection
                         , getAnswer    = postgresGetAnswer connection
                         , getQuestion  = postgresGetQuestion connection
                         }

-- | Get the parent question id for an answer id from postgres
postgresAnswerParent :: Connection -> AnswerId -> MaybeT IO QuestionId
postgresAnswerParent connection AnswerId{..} = do
  i <- lift $ concat <$>
               query connection
                    "SELECT questions.id FROM questions WHERE questions.id = (SELECT parent FROM answers where id = ?) "
                    [answerIdInt] 
  case i of
    [] -> MaybeT $ return Nothing
    qId:_ -> return $ QuestionId qId

-- | Get the tags for a question from postgres
postgresQuestionTags :: Connection -> QuestionId -> MaybeT IO (S.Set Text)
postgresQuestionTags connection QuestionId{..} = do
  i <- lift $ concat <$>
               query connection
                     "SELECT questions.tags FROM questions WHERE questions.id = ?"
                     [questionIdInt]
  case i of
    [] -> MaybeT $ return Nothing
    tags:_ -> return $ S.fromList $ V.toList tags


-- | Get an answer from postgres by id
postgresGetAnswer :: Connection -> AnswerId  -> MaybeT IO Answer
postgresGetAnswer connection AnswerId{..} = do
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
postgresGetQuestion :: Connection -> QuestionId  -> MaybeT IO Question
postgresGetQuestion connection QuestionId{..} = do
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