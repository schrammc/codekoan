{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Thesis.Data.Stackoverflow.Answer where

import Data.Aeson
import Data.Binary
import Data.Text

import Data.Maybe (fromMaybe)


import           Text.XML.Stream.Parse
import           Thesis.Data.Stackoverflow.XML.Helpers

data Answer = Answer { answerId :: !AnswerId
                     , answerBody :: !Text
                     , answerRating :: !Integer
                     , answerParent :: !Int
                     }
            deriving (Show, Eq)

instance FromJSON Answer where
  parseJSON (Object o) = Answer <$>
                  (AnswerId <$> o .: "answer_id") <*>
                  (fromMaybe "" <$> o .:? "body") <*>
                  o .: "score" <*>
                  o .: "question_id"

  parseJSON _ = fail "Expected a answer-json object, got a non-object json value!"


newtype AnswerId = AnswerId {answerIdInt :: Int}
                 deriving (Show, Eq, Ord)

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

data AnswerFragmentId = AnswerFragmentId { fragmentAnswerId :: AnswerId
                                         , fragmentId :: Int
                                         }
                        deriving (Show, Eq, Ord)

data AnswerFragmentMetaData =
  AnswerFragmentMetaData { fragmentMetaId :: AnswerFragmentId
                         , fragmentMetaSize :: Int
                         }
  deriving (Show, Eq, Ord)

fragmentMetaAnswerId :: AnswerFragmentMetaData -> AnswerId
fragmentMetaAnswerId AnswerFragmentMetaData{..} =
  fragmentAnswerId fragmentMetaId

fragmentMetaFragId :: AnswerFragmentMetaData -> Int
fragmentMetaFragId AnswerFragmentMetaData{..} =
  fragmentId fragmentMetaId
