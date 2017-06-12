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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Stackoverflow.Dictionary where

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Text
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.StackoverflowBodyParser (readCodeFromHTMLPost)
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question

import           Data.Vector ((!?))
import qualified Data.Vector as V

-- | A helper data structure that allows to find tags for all questions in a
-- stackoverflow data dump somewhat efficiently.
--
-- It also provides random access to Stackoverflow data.
--
-- Polymorphic over a monad that is used to access the given data. For database
-- access this could e.g. be 'IO'
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

-- | Get token vectors and normalized 'LanguageText' of all fragments of an
-- answer from the dictionary. This will return 'Nothing' if no answer for the
-- given id could be found.
answerFragments :: (Monad m, MonadLogger m) =>
                   DataDictionary m
                -> Language t l
                -> AnswerId
                -> MaybeT m (V.Vector (TokenVector t l, LanguageText l))
answerFragments dict@DataDictionary{..} lang@Language{..} aId = do
  txts <- answerFragmentTexts dict lang aId
  V.fromList <$> mapM (processFrag lang aId) txts

processFrag lang aId (fragId, txt) =
  case processAndTokenize lang txt of
    Nothing -> do
      $(logWarn) $ "Failed to parse answer " <>
                    (pack . show $ answerIdInt aId) <> " fragment " <>
                    (pack . show $ fragId)
      return (V.empty, txt)
    Just v  -> return (v, txt)

-- | Get token vectors and normalized 'LanguageText' of all fragments of an
-- answer from the dictionary. This will return 'Nothing' if no answer for the
-- given id could be found.
answerFragmentTexts :: (Monad m, MonadLogger m) =>
                   DataDictionary m
                -> Language t l
                -> AnswerId
                -> MaybeT m [(Int, LanguageText l)]
answerFragmentTexts DataDictionary{..} lang@Language{..} aId = do
  Answer{..} <- getAnswer aId
  let codeFragments = LanguageText <$> readCodeFromHTMLPost answerBody
  return $ (Prelude.zip [0..] codeFragments)


getAnswerFragment :: (Monad m, MonadLogger m) =>
                     DataDictionary m
                  -> Language t l
                  -> AnswerFragmentId
                  -> MaybeT m (TokenVector t l, LanguageText l)
getAnswerFragment dict lang AnswerFragmentId{..} = do
  txts <- answerFragmentTexts dict lang fragmentAnswerId
  case Prelude.drop fragmentId txts of
    (_, txt):_ -> processFrag lang fragmentAnswerId (fragmentId, txt)
    [] -> MaybeT $ return Nothing
--  MaybeT . return $ processFrag lang (fragmentAnswerId) txts !? V.fromList fragmentAnswerId
