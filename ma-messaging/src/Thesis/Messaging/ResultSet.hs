{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Thesis.Messaging.ResultSet where

import           Data.Aeson
import qualified Data.Map as M
import           Data.Text

import           GHC.Generics (Generic)

import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.ResultSet
import           Thesis.Messaging.Query

data ResultSetMsg =
  ResultSetMsg { resultSetLanguage :: Text
               , resultSetQueryId :: QueryId
               , resultSetResultList :: [ResultMsg]
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
  then Just $ ResultSetMsg { resultSetLanguage = resultSetLanguage r
                           , resultSetQueryId = resultSetQueryId r
                           , resultSetResultList =
                               mconcat $ resultSetResultList <$> rs
                           }
  else Nothing
  where
    langValid = and $ (\r' -> resultSetLanguage r == resultSetLanguage r') <$> rs
    queryIdValid = and $ (\r' -> resultSetQueryId r == resultSetQueryId r') <$> rs

resultSetToMsg :: Text -> QueryId -> ResultSet t l -> ResultSetMsg
resultSetToMsg lang replyTo ResultSet{..} = ResultSetMsg lang replyTo list
  where
    list = do
      (AnswerId{..}, mp) <- M.toList resultSetMap
      (fragId, matchGroups) <- M.toList mp
      group <- matchGroups

      let msgGroup = alignmentMatchToMsg <$> group
          sourceString = (show answerIdInt) ++ "(" ++ (show fragId) ++ ")"
      return $ ResultMsg (pack sourceString) msgGroup

data ResultMsg =
  ResultMsg { resultSource :: Text
            , resultAlignmentMatches :: [AlignmentMatchMsg]
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
