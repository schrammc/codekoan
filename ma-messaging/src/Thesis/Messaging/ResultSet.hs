{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.Messaging.ResultSet where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           Thesis.CodeAnalysis.Language
import           Thesis.Data.Range
import           Thesis.Messaging.Query
import           Thesis.Search.AlignmentMatch
import           Thesis.Search.FragmentData
import           Thesis.Search.ResultSet
import           Thesis.Util.VectorView

data ResultSetMsg =
  ResultSetMsg { resultSetLanguage :: !Text
               , resultSetQueryId :: !QueryId
               , resultSetResultList :: [ResultMsg]
               , resultClusterSize :: !Int
               , resultNumber :: !Int
               , resultQueryText :: !Text
               }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

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
               -- ^ Access to fragment content in DB
               -> Text
               -- ^ The query text
               -> m ResultSetMsg
resultSetToMsg clusterSize lang replyTo ResultSet{..} getTokenV queryText = do
  fragMap <- fragmentTexts
  let list = getMessageList fragMap
  return $!! ResultSetMsg lang replyTo list clusterSize 1 queryText
  where
    fragmentTexts :: (Monad m ) => m (M.HashMap fd (TokenVector t l, LanguageText l))
    fragmentTexts =  M.fromList . catMaybes <$> (
      forM (M.toList resultSetMap) $ \(ann, _) -> runMaybeT $ do
          (tokenV, txt) <- getTokenV ann
          return (ann, (tokenV, txt))
      )

    getMessageList :: (M.HashMap fd (TokenVector t l, LanguageText l)) -> [ResultMsg]
    getMessageList fragmentTextMap = do
      (ann, matchGroups) <- M.toList resultSetMap
      group <- matchGroups
      let (tokenV, fragText) = fromMaybe (V.empty, missingFragText) $ do
            (v,txt) <- M.lookup ann fragmentTextMap
            return $ (v, langText txt)
          missingFragText = "<<FRAGMENT TEXT NOT FOUND!>>"
          msgGroup = alignmentMatchToMsg (tokenV, fragText) <$> group
          sourceString = printFragData ann
      return $ ResultMsg (Text.pack sourceString) msgGroup fragText

data ResultMsg =
  ResultMsg { resultSource :: Text
            , resultAlignmentMatches :: [AlignmentMatchMsg]
            , resultFragmentText :: Text
            }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

data AlignmentMatchMsg =
  AlignmentMatchMsg { alignmentMatchLevenScore :: !Int
                    , alignmentMatchPatternTextRange :: (Range Text)
                    , alignmentMatchQueryTextRange :: (Range Text)
                    , alignmentMatchPatternTokenRange :: (Range Int)
                    , alignmentMatchQueryTokenRange :: (Range Int)
                    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

alignmentMatchToMsg :: (TokenVector t l, Text)
                    -> AlignmentMatch t l fd
                    -> AlignmentMatchMsg
alignmentMatchToMsg (v,_) AlignmentMatch{..} =
  AlignmentMatchMsg { alignmentMatchLevenScore =
                        resultLevenScore
                    , alignmentMatchQueryTextRange =
                        convertRange resultTextRange
                    , alignmentMatchPatternTextRange =
                        determinePatternTextRange v patternTokenRange
                    , alignmentMatchPatternTokenRange = patternTokenRange
                    , alignmentMatchQueryTokenRange =
                        convertRange resultQueryRange
                    }
  where
    patternTokenRange = convertRange resultFragmentRange
    determinePatternTextRange vec tokenRange =
      case vectorInRange tokenRange vec of
        Nothing -> Range 0 0
        Just matchedV ->
          let langTextRanges = coveredRange <$$$> matchedV
              start = minimum $ rangeStart <$> langTextRanges
              end = maximum $ rangeStart <$> langTextRanges
          in Range start end
