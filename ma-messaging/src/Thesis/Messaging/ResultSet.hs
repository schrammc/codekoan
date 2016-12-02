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
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.ResultSet
import           Thesis.Messaging.Query
import           Thesis.Data.Stackoverflow.Dictionary
import Thesis.CodeAnalysis.StackoverflowBodyParser
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

resultSetToMsg :: forall t l m . (Monad m) => Int
                  -- ^ The cluster size
               -> Text
               -- ^ The language that we're working with
               -> QueryId
               -- ^ The query that this result is is a response to
               -> ResultSet t l
               -- ^ The result set that we're persisting
               -> DataDictionary m
               -> Text
               -- ^ The query text
               -> m ResultSetMsg
resultSetToMsg clusterSize lang replyTo ResultSet{..} dict queryText = do
  fragMap <- fragmentTexts
  let list = getMessageList fragMap
  return $ ResultSetMsg lang replyTo list clusterSize 1 queryText
  where
    fragmentTexts :: (Monad m ) => m (M.Map (AnswerId, Int) Text)
    fragmentTexts =  M.fromList . Prelude.concat . catMaybes <$> (
      forM (M.toList resultSetMap) $ \(aId@AnswerId{..}, mp) -> runMaybeT $ do
          a <- getAnswer dict aId
          let fragTexts = readCodeFromHTMLPost $ answerBody a
          return $ (flip fmap) (M.toList mp) $
            \(fragId, _) -> ((aId, fragId), fragTexts !! fragId))

    getMessageList :: (M.Map (AnswerId, Int) Text) -> [ResultMsg]
    getMessageList fragmentTextMap = do
      (aId@AnswerId{..}, mp) <- M.toList resultSetMap
      (fragId, matchGroups) <- M.toList mp
      group <- matchGroups
      let fragText = fromMaybe "<<FRAGMENT TEXT NOT FOUND!>>"
                               (M.lookup (aId, fragId) fragmentTextMap)
      let msgGroup = alignmentMatchToMsg <$> group
          sourceString = (show answerIdInt) ++ "(" ++ (show fragId) ++ ")"
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
                    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

alignmentMatchToMsg :: AlignmentMatch t l -> AlignmentMatchMsg
alignmentMatchToMsg AlignmentMatch{..} =
  AlignmentMatchMsg resultLevenScore (convertRange resultTextRange)
