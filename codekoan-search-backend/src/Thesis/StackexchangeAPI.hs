{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.StackexchangeAPI where

import           Control.Exception
import           Control.Monad.Catch hiding (catch)
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector ((!?))
import qualified Data.Vector as V
import           Thesis.StackexchangeAPI.WrapperParseException

import           Network.HTTP.Client.Conduit (defaultManagerSettings)
import           Network.HTTP.Conduit

import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question
import           Thesis.Data.Stackoverflow.SOAccess

import           Thesis.StackexchangeAPI.WrapperObject


apiVersion :: String
apiVersion = "2.2"

data CrawlerConfig = CrawlerConfig { crawlerKey :: Text
                                   , crawlerFilter :: Text
                                   , crawlerSite :: Text
                                   , crawlerRetries :: Int
                                   , crawlerTimeout :: Int -- ^ In seconds
                                   }

defaultCrawler :: CrawlerConfig
defaultCrawler = CrawlerConfig { crawlerKey = "LPrwv6vi2NKybcgYvVkBbA(("
                               , crawlerFilter = "!6YQ-CANnCkDp9zqEgNchXd050GozzP3Q_2OpR_LG1KXja26oxMEI1v(zn0GOA5"
                               , crawlerSite = "stackoverflow"
                               , crawlerRetries = 4
                               , crawlerTimeout = 120
                               }
allFields :: [String]
allFields = [ ".backoff"
            , ".error_id"
            , ".error_message"
            , ".error_name"
            , ".has_more"
            , ".items"
            , ".page"
            , ".page_size"
            , ".quota_max"
            , ".quota_remaining"
            , ".total"
            , ".type"

            , "question.accepted_answer_id"
            , "question.answers"
            , "question.answer_count"
            , "question.body"
            , "question.closed_date"
            , "question.closed_reason"
            , "question.comments"
            , "question.comment_count"
            , "question.creation_date"
            , "question.is_answered"
            , "question.owner"
            , "question.question_id"
            , "question.link"
            , "question.score"
            , "question.title"
            , "question.tags"
            , "question.view_count"

            , "answer.answer_id"
            , "answer.question_id"
            , "answer.body"
            , "answer.link"
            , "answer.owner"
            , "answer.score"
            , "answer.comments"
            , "answer.creation_date"

            , "comment.comment_id"
            , "comment.body"
            , "comment.link"
            , "comment.owner"
            , "comment.score"
            , "comment.creation_date"

            , "shallow_user.user_id"
            , "shallow_user.user_type"
            , "shallow_user.reputation"
            , "shallow_user.accept_rate"
            , "shallow_user.display_name"
            , "shallow_user.link"

            , "filter.filter"
            , "filter.filter_type"
            , "filter.included_fields"
            ]

-- | A handle to access Stackoverflow-Data through the API
soAPIAccess :: SOAccess
soAPIAccess = SOAccess { getSOQuestion = requestQuestion defaultCrawler
                       , getSOAnswer = requestAnswer defaultCrawler
                       , getSOAnswersTo = requestAnswersTo defaultCrawler}

-- | Get a question with a given ID from Stackoverflow
requestQuestion :: CrawlerConfig -> QuestionId -> IO (Maybe Question)
requestQuestion conf qId = do
  resultObject <- requestQuestionWrapper conf qId
  return $ do
    filterObj <- wrapperItems resultObject !? 0
    Aeson.parseMaybe Aeson.parseJSON filterObj

-- | Get an answer with the given ID from stackoverflow
requestAnswer :: CrawlerConfig -> AnswerId -> IO (Maybe Answer)
requestAnswer CrawlerConfig{..} AnswerId{..} = do
  request <- parsedRequest
  manager <- newManager defaultManagerSettings
  resultObject <- queryStackoverflow request manager
  return $ do
    filterObj <- wrapperItems resultObject !? 0
    Aeson.parseMaybe Aeson.parseJSON filterObj
  where
    parsedRequest = parseUrl $ baseURL ++ filterParams ++ siteParam
    filterParams = "&base=none&unsafe=false" ++
                   "&filter=" ++ Text.unpack crawlerFilter
    siteParam = "&site=" ++ Text.unpack crawlerSite
    baseURL = "http://api.stackexchange.com/" ++ apiVersion ++ "/answers/"
              ++ (show answerIdInt) ++ "/?"

-- | List the ID's of answers to the given question
requestAnswersTo :: CrawlerConfig -> QuestionId -> IO (Maybe [AnswerId])
requestAnswersTo conf qId = do
  resultObject <- requestQuestionWrapper conf qId
  return $ do
    filterObj <- wrapperItems resultObject !? 0
    Aeson.parseMaybe (Aeson.withObject "questionObject" $ \o ->
                       (fmap answerId) <$> (o .: "answers" :: Aeson.Parser [Answer])
                       ) filterObj

-- | Request a question and return the reply - wrapper object unmodified.
requestQuestionWrapper :: CrawlerConfig -> QuestionId -> IO WrapperObject
requestQuestionWrapper CrawlerConfig{..} QuestionId{..} = do
  request <- parsedRequest
  manager <- newManager defaultManagerSettings
  queryStackoverflow request manager
  where
    parsedRequest = parseUrl $ baseURL ++ filterParams ++ siteParam
    filterParams = "&base=none&unsafe=false" ++
                   "&filter=" ++ Text.unpack crawlerFilter
    siteParam = "&site=" ++ Text.unpack crawlerSite
    baseURL = "http://api.stackexchange.com/" ++ apiVersion ++ "/questions/"
              ++ (show questionIdInt) ++ "/?"

-- | Send a REST query to the stackexchange api and expect to get a
-- stackexchange wrapper object back. Throws a 'FailedConnectionException'
-- exception if something goes wrong with the connection or a
-- 'WrapperParseException' if the returned JSON can't be parsed.
queryStackoverflow :: Request -> Manager -> IO WrapperObject
queryStackoverflow request manager = do
  reply <- responseBody <$> httpLbs request manager
  let jsonResult = Aeson.decode reply
  case jsonResult of
    Nothing -> throwIO $ WrapperParseException "Failed to decode JSON" reply
    Just jsonObj -> do
      let wrapperRes = Aeson.parseEither Aeson.parseJSON jsonObj
      case wrapperRes of
        Right x -> return x
        Left err -> throwIO $ WrapperParseException err reply

-- | A helper function to create a stackexchange API filter and get back it's
-- ID. Throws a 'FailedConnectionException' if there are problems with the
-- connection. Throws a 'WrapperParseException' if the reply from Stackoverflow
-- couldn't be parsed right.
createStackoverflowFilter :: [String] -> IO (Maybe Text)
createStackoverflowFilter fields = do
  request <- parseRequest
  manager <- newManager defaultManagerSettings
  resultObject <- queryStackoverflow request manager
  return $ do
    filterObj <- wrapperItems resultObject !? 0
    Aeson.parseMaybe readFilterField filterObj
  where
    readFilterField =  Aeson.withObject "filter" (.: "filter")
    parseRequest = parseUrl $ baseURL ++ includeParam ++ filterParams
    filterParams = "&base=none&unsafe=false" ++
                   "&filter=!nX1lUomT*pBWhgLUsTDIoQl5BCyD.ALq7" ++
                     "Ne2(AvlIoN5Cx)Pg(*OBFa)LIL"
    includeParam = "&include=" ++ (concat $ intersperse ";" fields)
    baseURL = "http://api.stackexchange.com/" ++ apiVersion ++ "/filters/create?"

-- | Parse the items in a 'WrapperObject' or throw a 'WrapperParseException' if
-- one fails
parseItemsOrThrow :: (Aeson.FromJSON a, MonadThrow m)
                     => WrapperObject
                     -> m (V.Vector a)
parseItemsOrThrow w =
  case V.sequence $ wrapperParseItems w of
    Just xs -> return xs
    Nothing -> throwM $ WrapperParseException
                         "Failed to parse questions!"
                         "-- None supplied--"
