{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Dump.Source.Postgres where

import Thesis.Data.Stackoverflow.Answer

import Database.PostgreSQL.Simple

import Data.Conduit hiding (connect)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
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
