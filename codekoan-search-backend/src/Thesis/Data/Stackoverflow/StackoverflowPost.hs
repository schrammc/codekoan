-- |
-- Description: Data structures and functions for SO posts (questions and answers)
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.StackoverflowPost
       ( -- * Data types
         StackoverflowPost(..)
         -- ** Accesor functions
       , isQuestion
       , isAnswer
       , questionOrAnswer
       , postRating
         -- ** Access to post's code
       , codeLength
       , ratingAndLength
         -- * Conduit helpers
       , filterQuestions
       , filterAnswers
         -- * Parsing
       , parsePosts
       )where

import           Control.Monad (void)
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Text as Text


import           Data.XML.Types (Event)
import           Text.XML.Stream.Parse
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question
import           Thesis.Data.Stackoverflow.XML.Helpers
import Thesis.CodeAnalysis.StackoverflowBodyParser

data StackoverflowPost = PostQuestion !Question
                       | PostAnswer !Answer
                       | PostUnknown
                       deriving (Show, Eq)

-- | Predicate to determine if a post is a stack overflow question
isQuestion :: StackoverflowPost -> Bool
isQuestion (PostQuestion _) = True
isQuestion _ = False

-- | Predicate to determine if a post is a stack overflow answer
isAnswer :: StackoverflowPost -> Bool
isAnswer (PostAnswer _) = True
isAnswer _ = False

-- | Predicate to determine if a post is an answer or a question (as opposed to
-- some other unknown category)
questionOrAnswer :: StackoverflowPost -> Bool
questionOrAnswer PostUnknown = False
questionOrAnswer _           = True

-- | Rating of a post
--
-- / This only returns a defined integer if the given post is a question or an
-- answer/
postRating :: StackoverflowPost -> Maybe Integer
postRating (PostQuestion Question{..}) = Just questionRating
postRating (PostAnswer Answer{..}) = Just answerRating
postRating _ = Nothing

-- | Length of the longest code fragment in a stackoverflow post
--
-- /This only returns a defined 'Int' value if the post is a 'Question' or an
-- 'Answer' and it contains nonempty code tags/
codeLength :: StackoverflowPost -> Maybe Int
codeLength (PostQuestion Question{..}) =
  case readCodeFromHTMLPost questionBody of
    [] -> Nothing
    _ -> Just $ (foldl1 max) $ Text.length <$> readCodeFromHTMLPost questionBody
codeLength (PostAnswer Answer{..}) =
  case readCodeFromHTMLPost answerBody of
    [] -> Nothing
    _ -> Just $ (foldl1 max) $ Text.length <$> readCodeFromHTMLPost answerBody
codeLength _ = Nothing

-- | For a post that has code, this function yields the post's rating and the
-- length of it's longest code fragment
ratingAndLength :: StackoverflowPost -> Maybe (Integer, Integer)
ratingAndLength p = do
  l <- codeLength p
  r <- postRating p
  return (r,fromIntegral l)

--------------------------------------------------------------------------------
--
-- Conduit helpers

-- | Only process stack overflow answers further
filterAnswers :: Monad m => Conduit StackoverflowPost m Answer
filterAnswers =  CL.filter isAnswer =$= CL.map (\(PostAnswer a) -> a)

-- | Only process stack overflow questions further
filterQuestions :: Monad m => Conduit StackoverflowPost m Question
filterQuestions =  CL.filter isQuestion =$= CL.map (\(PostQuestion q) -> q)


--------------------------------------------------------------------------------
--
-- Parsing functions

data PostType = AnswerType | QuestionType | OtherType
              deriving (Show, Eq)


-- | Read a text encoded post type where 1 is a question, 2 is an answer and
-- everything else is "other"
readPostType :: Text.Text -> PostType
readPostType "1" = QuestionType
readPostType "2" = AnswerType
readPostType _ = OtherType

parsePosts :: ConduitM Event StackoverflowPost (ResourceT IO) ()
parsePosts = void $ tagNoAttr "posts" $ manyYield parsePost

parsePost :: ConduitM Event o (ResourceT IO) (Maybe StackoverflowPost)
parsePost = tag "row" (\_ -> parseAttributes) $ return
  where
    parseAttributes = do
      rowId <- readAttribute "Id"
      postType <- readPostType <$> requireAttr "PostTypeId"
      postBody <- requireAttr "Body"
      content <- case postType of
        AnswerType -> PostAnswer <$> parseAnswer rowId postBody
        QuestionType -> PostQuestion <$> parseQuestion rowId postBody
        OtherType -> return PostUnknown
      ignoreAttrs
      return content
