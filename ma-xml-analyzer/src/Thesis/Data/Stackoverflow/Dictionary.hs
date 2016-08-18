-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation independent data strucutre to access
-- stackoverflow data.
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Data.Stackoverflow.Dictionary where

import Data.Text
import qualified Data.Set as S
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.StackoverflowBodyParser (readCodeFromHTMLPost)
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Control.Monad.Trans.Maybe
import Control.Monad.Catch

import Data.Vector ((!?))
import qualified Data.Vector as V

-- | A helper data structure that allows to find tags for all questions in a
-- stackoverflow data dump somewhat efficiently.
--
-- Polymorphic over a monad that is used to access the given data
data DataDictionary m where
  DataDictionary :: (MonadThrow m) =>
                    { answerParent :: AnswerId -> MaybeT m QuestionId
                      -- ^ Each answer has a parent question. Get this
                      -- question's id.
                    , questionTags :: QuestionId -> MaybeT m (S.Set Text)
                      -- ^ Get the set of tags for a question
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

-- | Get the token vectors of all answer code fragments of an answer from the
-- dictionary for one answer. This will return 'Nothing' if no answer for the
-- given id could be found.
answerFragments :: (Monad m) =>
                   DataDictionary m
                -> Language t l
                -> AnswerId
                -> MaybeT m (V.Vector (TokenVector t l))
answerFragments DataDictionary{..} lang@Language{..} aId = do
  Answer{..} <- getAnswer aId
  let codeFragments = LanguageText <$> readCodeFromHTMLPost answerBody
  V.fromList <$> mapM process codeFragments
  where
    process txt =  MaybeT . return $ processAndTokenize lang txt

-- | Get the token vector of one specific answer code fragment from the
-- dictionary. This will return 'Nothing' if the answer code fragment couldn't
-- be found.
answerFragmentTokens  :: (Monad m) =>
                         DataDictionary m
                      -> Language t l
                      -> AnswerFragmentId
                      -> MaybeT m (TokenVector t l)
answerFragmentTokens dict lang AnswerFragmentId{..} = do
  fragmentTokens <- answerFragments dict lang fragmentAnswerId
  MaybeT $ return (fragmentTokens !? fragmentId)
