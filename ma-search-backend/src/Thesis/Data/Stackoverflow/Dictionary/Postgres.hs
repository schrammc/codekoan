-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A 'DataDictionary' implementation using a PostgresSQL backend
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Thesis.Data.Stackoverflow.Dictionary.Postgres where

import           Control.Concurrent.MVar
import           Control.Monad.Logger
import           Control.Monad.Catch

import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import           Data.Vector as V (toList)

import           Database.PostgreSQL.Simple as PSQL

import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question

import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Control.Monad.Catch

import qualified Data.Set as S (fromList, Set)

-- | A 'DataDictionary' that accesses a PostgresSQL database.
--
-- On Sql errors, retry is attempted three times before giving up and throwing
-- the error a level outwards.
postgresDictionary :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m) =>
                      ConnectInfo
                   -> m (DataDictionary m)
postgresDictionary connectInfo = do
  connection <- liftIO $ PSQL.connect connectInfo
  cVar <- liftIO $ newMVar connection
  return $ DataDictionary{ answerParent = \aId -> 
                             withPsqlConnection cVar
                                                (\c -> postgresAnswerParent c aId)
                         , questionTags = \qId ->
                             withPsqlConnection cVar
                                                (\c -> postgresQuestionTags c qId)
                         , getAnswer    = \qId ->
                             withPsqlConnection cVar
                                                (\c -> postgresGetAnswer c qId)
                         , getQuestion  = \aId ->
                             withPsqlConnection cVar
                                                (\c -> postgresGetQuestion c aId)
                         }
  where
    withPsqlConnection connVar action = do
      conn <- liftIO $ takeMVar connVar
      result <- catch (doWithConnection conn) (\(e :: SqlError) -> retry 3 e)
      return result
      where
        doWithConnection connection = do
          result <- action connection
          liftIO $ putMVar connVar connection
          return result
        retry n e = do
          connection <- liftIO $ PSQL.connect connectInfo
          $(logWarn) $ "SqlError: Reconnecting and retrying (" <>
                       (pack $ show n) <> ")"
          $(logWarn) $ "SqlError message:" <> (pack $ show e)
          catch (doWithConnection connection) $ \(e :: SqlError) ->
                if n <= 0
                  then do
                    $(logError) "Too many retries, connection broken!"
                    throwM e
                  else retry (n-1) e

                

  
-- | Get the parent question id for an answer id from postgres
postgresAnswerParent :: (MonadIO m) =>
                        Connection
                     -> AnswerId
                     -> MaybeT m QuestionId
postgresAnswerParent connection AnswerId{..} = do
  i <- liftIO $ concat <$>
               query connection
                    "SELECT questions.id FROM questions WHERE questions.id = (SELECT parent FROM answers where id = ?) "
                    [answerIdInt] 
  case i of
    [] -> MaybeT $ return Nothing
    qId:_ -> return $ QuestionId qId

-- | Get the tags for a question from postgres
postgresQuestionTags :: (MonadIO m) =>
                        Connection
                     -> QuestionId
                     -> MaybeT m (S.Set Text)
postgresQuestionTags connection QuestionId{..} = do
  i <- liftIO $ concat <$>
               query connection
                     "SELECT questions.tags FROM questions WHERE questions.id = ?"
                     [questionIdInt]
  case i of
    [] -> MaybeT $ return Nothing
    tags:_ -> return $ S.fromList $ V.toList tags


-- | Get an answer from postgres by id
postgresGetAnswer :: (MonadIO m) =>
                     Connection
                  -> AnswerId
                  -> MaybeT m Answer
postgresGetAnswer connection AnswerId{..} = do
  results <- liftIO $ query connection
                          "SELECT * FROM answers WHERE answers.id = ?"
                          [answerIdInt]
  let answers = do
        (aId, aBody, aRating, aParent) <- results
        return $ Answer (AnswerId aId)
                        aBody
                        aRating
                        aParent
  case length answers `seq` answers of
    [] -> MaybeT $ return Nothing
    (a:_) -> return a
  
-- | Get a question from postgres by id
postgresGetQuestion :: (MonadIO m) => Connection
                    -> QuestionId
                    -> MaybeT m Question
postgresGetQuestion connection QuestionId{..} = do
  results <- liftIO $ query connection
                          "SELECT * FROM questions WHERE questions.id = ?"
                          [questionIdInt]
  let questions = do
        (qId, qTags, qBody, qTitle, qRating) <- results
        return $ Question (QuestionId qId)
                          qBody
                          qTitle
                          qRating
                          qTags
  case length questions `seq` questions of
    [] -> MaybeT $ return Nothing
    (q:_) -> return q
