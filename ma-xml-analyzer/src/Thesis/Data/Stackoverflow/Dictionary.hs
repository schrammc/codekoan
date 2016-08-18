{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module Thesis.Data.Stackoverflow.Dictionary where

import Data.Text
import qualified Data.Set as S
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Control.Monad.Trans.Maybe
import Control.Monad.Catch

-- | A helper data structure that allows to find tags for all questions in a
-- stackoverflow data dump somewhat efficiently.
--
-- Polymorphic over a monad that is used to access the given data
data DataDictionary m where
  DataDictionary :: (MonadThrow m) =>
                    { answerParent :: AnswerId -> MaybeT m QuestionId
                    , questionTags :: QuestionId -> MaybeT m (S.Set Text)
                    , getQuestion  :: QuestionId -> MaybeT m Question
                    , getAnswer    :: AnswerId -> MaybeT m Answer
                    } -> DataDictionary m

-- | Get the tags of the parent question to the given answer
--
-- Nothing if the given 'AnswerId' is not in the dictionary
answerRootTags :: DataDictionary m -> AnswerId -> MaybeT m (S.Set Text)
answerRootTags DataDictionary{..} aId = do
  parent <- answerParent aId
  questionTags parent

-- | See if the answer's parent question is tagged with the given tag
answerWithTag :: DataDictionary m -> Text -> AnswerId -> m Bool
answerWithTag dict@DataDictionary{..} tag aId = do
  maybeTags <- runMaybeT $ answerRootTags dict aId
  case maybeTags of
    Nothing -> return False
    Just tags -> return $ S.member tag tags
