{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Data.Stackoverflow.Answer where

import Control.DeepSeq

import Data.Aeson
import Data.Binary
import Data.Text
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe (fromMaybe)


import           Text.XML.Stream.Parse
import           Thesis.Data.Stackoverflow.XML.Helpers

data Answer = Answer { answerId :: {-# UNPACK #-} !AnswerId
                     , answerBody :: !Text
                     , answerRating :: !Integer
                     , answerParent :: {-# UNPACK #-}!Int
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
                 deriving (Show, Eq, Ord, Generic, Hashable)

instance Binary AnswerId where
  put AnswerId{..} = put answerIdInt
  get = AnswerId <$> get

-- | Parse an answer from a post element of the Stackoverflow data dump given
-- the answer's id and the body of the question as 'Text'
parseAnswer :: Int -> Text -> AttrParser Answer
parseAnswer idInt body= do
  score  <- readAttribute "Score"
  parent <- readAttribute "ParentId"
  return $! Answer (AnswerId idInt) body score parent

data AnswerFragmentId =
  AnswerFragmentId { fragmentAnswerId :: {-# UNPACK #-} !AnswerId
                   , fragmentId :: {-# UNPACK #-} !Int
                   }
                        deriving (Show, Eq, Ord, Generic, Hashable)

instance NFData AnswerFragmentId where
  rnf AnswerFragmentId{..} = ()

data AnswerFragmentMetaData =
  AnswerFragmentMetaData { fragmentMetaId :: {-# UNPACK #-} !AnswerFragmentId
                         , fragmentMetaSize :: {-# UNPACK #-} !Int
                         }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance NFData AnswerFragmentMetaData where
  rnf AnswerFragmentMetaData{..} = deepseq fragmentMetaId ()

fragmentMetaAnswerId :: AnswerFragmentMetaData -> AnswerId
fragmentMetaAnswerId AnswerFragmentMetaData{..} =
  fragmentAnswerId fragmentMetaId

fragmentMetaFragId :: AnswerFragmentMetaData -> Int
fragmentMetaFragId AnswerFragmentMetaData{..} =
  fragmentId fragmentMetaId
