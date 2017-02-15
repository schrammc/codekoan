{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Messaging.ResultSet where

import           Data.Aeson
import qualified Data.Map as M
import           Data.Text
import           Data.Maybe (catMaybes, fromMaybe)
import           Control.Monad
import           GHC.Generics (Generic)
import           Thesis.Data.Range
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.ResultSet
import           Thesis.Search.FragmentData
import           Thesis.Messaging.Query
import           Thesis.CodeAnalysis.Language
import Control.Monad.Trans.Maybe

data ResultSetMsg =
  ResultSetMsg { resultSetLanguage :: !Text
               , resultSetQueryId :: !QueryId
               , resultSetResultList :: [ResultMsg]
               , resultClusterSize :: !Int
               , resultNumber :: !Int
               , resultQueryText :: !Text
               }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Merge a list of result sets. It is checked if these 'ResultSetMsg's all
-- have the same language.
--
-- Yields 'Nothing' if either
--
--    * The given list is empty
--    * The 'ResultSetMsg's in the given list don't have the same language
--    * The 'ResultSetMsg's in the given list don't reply to the same query
mergeResultSetMsgs :: [ResultSetMsg] -> Maybe ResultSetMsg
mergeResultSetMsgs [] = Nothing
mergeResultSetMsgs rs@(r:_) =
  if langValid && queryIdValid
  then Just $ r{ resultSetResultList = mconcat $ resultSetResultList <$> rs
               , resultNumber = newNumber
               }
  else Nothing
  where
    langValid = and $ (\r' -> resultSetLanguage r == resultSetLanguage r') <$> rs
    queryIdValid = and $ (\r' -> resultSetQueryId r == resultSetQueryId r') <$> rs
    newNumber = sum $ resultNumber <$> rs

resultSetToMsg :: forall t l m fd . (FragmentData fd, Monad m) => Int
                  -- ^ The cluster size
               -> Text
               -- ^ The language that we're working with
               -> QueryId
               -- ^ The query that this result is is a response to
               -> ResultSet t l fd
               -- ^ The result set that we're persisting
               -> (fd -> MaybeT m (TokenVector t l, LanguageText l))
               -> Text
               -- ^ The query text
               -> m ResultSetMsg
resultSetToMsg clusterSize lang replyTo ResultSet{..} getTokenV queryText = do
  fragMap <- fragmentTexts
  let list = getMessageList fragMap
  return $ ResultSetMsg lang replyTo list clusterSize 1 queryText
  where
    fragmentTexts :: (Monad m ) => m (M.Map fd Text)
    fragmentTexts =  M.fromList . catMaybes <$> (
      forM (M.toList resultSetMap) $ \(ann, _) -> runMaybeT $ do
          (_,txt) <- getTokenV ann
          return (ann, langText txt)
      )

    getMessageList :: (M.Map fd Text) -> [ResultMsg]
    getMessageList fragmentTextMap = do
      (ann, matchGroups) <- M.toList resultSetMap
      group <- matchGroups
      let fragText = fromMaybe "<<FRAGMENT TEXT NOT FOUND!>>"
                               (M.lookup ann fragmentTextMap)
      let msgGroup = alignmentMatchToMsg <$> group
          sourceString = printFragData ann
      return $ ResultMsg (pack sourceString) msgGroup fragText

data ResultMsg =
  ResultMsg { resultSource :: Text
            , resultAlignmentMatches :: [AlignmentMatchMsg]
            , resultFragmentText :: Text
            }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AlignmentMatchMsg =
  AlignmentMatchMsg { alignmentMatchLevenScore :: !Int
                    , alignmentMatchResultTextRange :: (Range Text)
                    , alignmentMatchPatternTokenRange :: (Range Int)
                    , alignmentMatchQueryTokenRange :: (Range Int)
                    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

alignmentMatchToMsg :: AlignmentMatch t l fd -> AlignmentMatchMsg
alignmentMatchToMsg AlignmentMatch{..} =
  AlignmentMatchMsg resultLevenScore
                    (convertRange resultTextRange)
                    (convertRange resultFragmentRange)
                    (convertRange resultQueryRange)
