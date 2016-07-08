{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Question where

import Data.Attoparsec.Text as AP

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text

import qualified Data.Vector as V

import           Text.XML.Stream.Parse
import           Thesis.Data.Stackoverflow.XML.Helpers

data Question = Question { questionId :: !QuestionId
                         , questionBody :: !Text
                         , questionTitle :: !Text
                         , questionRating :: !Integer
                         , questionTags :: !(V.Vector Text)
                         }
              deriving (Show, Eq)
                       
newtype QuestionId = QuestionId {questionIdInt :: Int}
                   deriving (Show, Eq)

instance FromJSON Question where
  parseJSON (Object o) = Question <$>
                  (QuestionId <$> o .: "question_id") <*>
                  (fromMaybe "" <$> o .:? "body") <*>
                  o .: "title" <*>
                  o .: "score" <*>
                  o .: "tags"
  parseJSON _ = fail "Expected a answer-json object, got something else!"
                            
-- | Parse a question from a post element of the Stackoverflow data dump given
-- the question's id and the body of the question as 'Text'
parseQuestion :: Int -> Text -> AttrParser Question
parseQuestion idInt body  = do
  questionTitle <- requireAttr "Title"
  score <- readAttribute "Score"
  tags <- parseWithDefault V.empty parseTags <$> requireAttr "Tags"
  return $ Question (QuestionId idInt) body questionTitle score tags

parseTags :: Parser (V.Vector Text)
parseTags = V.fromList <$> AP.many' tag
  where
    tag = do
      char '<'
      content <- AP.takeWhile (/= '>')
      char '>'
      return content
