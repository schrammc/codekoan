{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Data.Stackoverflow.Answer where

import Data.Text

import Data.Binary

import           Text.XML.Stream.Parse
import           Thesis.Data.Stackoverflow.XML.Helpers

data Answer = Answer { answerId :: !AnswerId
                     , answerBody :: !Text
                     , answerRating :: !Integer
                     , answerParent :: !Int
                     }
            deriving (Show, Eq)

newtype AnswerId = AnswerId {answerIdInt :: Int}
                 deriving (Show, Eq)

instance Binary AnswerId where
  put AnswerId{..} = put answerIdInt
  get = AnswerId <$> get

-- | Parse an answer from a post element of the Stackoverflow data dump given
-- the answer's id and the body of the question as 'Text'
parseAnswer :: Int -> Text -> AttrParser Answer
parseAnswer idInt body= do
  score  <- readAttribute "Score"
  parent <- readAttribute "ParentId"
  return $ Answer (AnswerId idInt) body score parent

