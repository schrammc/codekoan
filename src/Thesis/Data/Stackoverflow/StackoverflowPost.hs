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
import           Data.Attoparsec.Text as AP
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as V
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

isQuestion :: StackoverflowPost -> Bool
isQuestion (PostQuestion _) = True
isQuestion _ = False

isAnswer :: StackoverflowPost -> Bool
isAnswer (PostAnswer _) = True
isAnswer _ = False

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
    
filterAnswers :: Monad m => Conduit StackoverflowPost m Answer
filterAnswers =  CL.filter isQuestion =$= CL.map (\(PostAnswer a) -> a)


filterQuestions :: Monad m => Conduit StackoverflowPost m Question
filterQuestions =  CL.filter isQuestion =$= CL.map (\(PostQuestion q) -> q)


--------------------------------------------------------------------------------
--
-- Parsing functions

data PostType = AnswerType | QuestionType | OtherType
              deriving (Show, Eq)


                       
readPostType "1" = QuestionType
readPostType "2" = AnswerType
readPostType _ = OtherType

parsePosts :: ConduitM Event StackoverflowPost (ResourceT IO) ()
parsePosts = void $ tagNoAttr "posts" $ manyYield parsePost

parsePost :: ConduitM Event o (ResourceT IO) (Maybe StackoverflowPost)
parsePost = tagName "row" parseAttributes $ return
  where
    parseAttributes = do
      rowId <- readAttribute "Id"
      postType <- readPostType <$> requireAttr "PostTypeId"
      postBody <- requireAttr "Body"
      content <- case postType of
        AnswerType -> PostQuestion <$> parseQuestion rowId postBody
        QuestionType -> PostAnswer <$> parseAnswer rowId postBody
        OtherType -> return PostUnknown
      ignoreAttrs
      return content
