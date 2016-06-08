{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.Data.Stackoverflow.Dictionary where

import Data.Text
import Data.Maybe
import Data.Binary
import GHC.Generics (Generic)

import qualified Data.Set as S

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Thesis.Data.Stackoverflow.Answer

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
                      deriving Generic

instance Binary DataDictionary

         
-- | Get the text tags for an answer's parent question
answerRootTags :: DataDictionary -> AnswerId -> Maybe (S.Set Text)
answerRootTags (DataDictionary{..}) (AnswerId aid) = do
  parent <- IM.lookup aid dictAnswerParents
  tags <- IM.lookup parent dictQuestionTags
  tagStrings <- mapM (\tag -> IM.lookup tag dictTagTable) (S.toList tags)
  return $ S.fromList tagStrings

-- | See if the answer's parent question is tagged with the given tag
answerWithTag :: DataDictionary -> Text -> AnswerId -> Bool
answerWithTag dict tag i = maybe False (S.member tag) (answerRootTags dict i)
