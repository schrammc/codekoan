{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Data.Stackoverflow.Dictionary where

import Data.Text hiding (concat)

import Data.Binary
import GHC.Generics (Generic)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Set as S

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question

import Control.DeepSeq

-- | A helper data structure that allows to find tags for all questions in a
-- stackoverflow data dump somewhat efficiently.
--
-- A binary instance is provided, so that the data can be persisted in binary
-- form to a file, as recomputing it for repeated applications is prohibitively
-- expensive.
data DataDictionary = DataDictionary { dictAnswerParents :: !(IntMap Int)
                                     , dictQuestionTags :: !(IntMap (S.Set Int))
                                     , dictTagTable :: !(IntMap Text)
                                     }
                      deriving (Generic, NFData, Binary)
         
-- | Get the text tags for an answer's parent question
answerRootTags :: DataDictionary -> AnswerId -> Maybe (S.Set Text)
answerRootTags (DataDictionary{..}) (AnswerId aid) = do
  parent <- IM.lookup aid dictAnswerParents
  tags <- IM.lookup parent dictQuestionTags
  tagStrings <- mapM (\tag -> IM.lookup tag dictTagTable) (S.toList tags)
  return $ S.fromList tagStrings

-- | Get the text tags for an answer's parent question
answerParent :: DataDictionary -> AnswerId -> Maybe (QuestionId)
answerParent (DataDictionary{..}) (AnswerId aid) = do
  parent <- IM.lookup aid dictAnswerParents
  return $ QuestionId parent

-- | See if the answer's parent question is tagged with the given tag
answerWithTag :: DataDictionary -> Text -> AnswerId -> Bool
answerWithTag dict tag i = maybe False (S.member tag) (answerRootTags dict i)

readDictionary :: FilePath -> IO DataDictionary
readDictionary dictPath = do
  content <- BS.readFile dictPath
  return $!! decode (BL.fromChunks [content])

emptyDictionary = DataDictionary IM.empty IM.empty IM.empty
