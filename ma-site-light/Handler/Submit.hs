-- |
-- Author: Christof Schramm
-- License: All rights reserved
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Submit where

import Import

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Simple

import Control.Monad.Trans.Maybe

import Text.Printf
import qualified Data.Text as Text

import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Java
import Thesis.CodeAnalysis.Language.Python

import Thesis.Search.Settings

import Thesis.Data.Range

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question

import Thesis.Data.Stackoverflow.Dictionary as Dict

import Thesis.Messaging.Message
import Thesis.Messaging.Query
import Thesis.Messaging.ResultSet

import Prelude (read)
import Data.Char (isDigit)

getSubmitR :: Handler Html
getSubmitR = do
  (formWidget, formEnctype) <- generateFormPost submitCodeForm
  let resultWidgetMaybe = Nothing  :: Maybe Widget
  defaultLayout $ do
    setTitle "Code Analysis"
    $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
  ((formResult, formWidget), formEnctype) <- runFormPost submitCodeForm
  App{..} <- getYesod

  resultWidgetMaybe <- case formResult of
    FormSuccess (code, language, settings) -> do

      qId <- submitQuery appSettings (code, language, settings)
      searchReply <- waitForReply appSettings qId

      let code' = if language == "java"
                  then langText $ normalize java (LanguageText code)
                  else langText $ normalize python (LanguageText code)

      resultW <- resultWidget appDict code' searchReply

      return . Just $ resultW
    _ -> return . Just $ [whamlet| <div .alert .alert-danger>
                                     <center> <b>
                                       Invalid input |]

  defaultLayout $ do
    setTitle "Code Analysis"
    $(widgetFile "submit")

-- | Submit a query to the rabbitmq injector and return the resulting query id
submitQuery :: (MonadIO m, MonadLogger m) =>
               AppSettings
            -> (Text, Text, SearchSettings)
            -> m QueryId
submitQuery AppSettings{..} (code, language, settings) = do
  $(logDebug) $ (Text.pack $ show settings)
  let query = buildQuery code language settings
      submitR = parseRequest_ appSubmitURL

      submitRWithBody = setRequestBodyJSON query $
                        submitR{ method = methodPost }

  submissionResponse <- httpJSON submitRWithBody

  let qidParser = withObject "reply object" $ \o -> o .: "queryId"
      qIdMaybe  = parseMaybe qidParser $ getResponseBody submissionResponse

  when (qIdMaybe == Nothing) $ fail "Failed to parse rmq injector reply!"

  let Just qId = qIdMaybe :: Maybe QueryId


  $(logDebug) $ "Submitted query with id " <> (pack $ show qId)
  
  return qId

-- | Wait until there is a reply for the given query id in the reply-cache
waitForReply :: (MonadIO m, MonadLogger m) =>
                AppSettings
             -> QueryId
             -> m (Either String ResultSetMsg)
waitForReply settings@AppSettings{..} queryId@(QueryId qId) = do
  resp <- httpJSON req 
  
  let statusParser = withObject "reply object" $ \o -> o .: "status"
      statusMaybe = parseMaybe statusParser $ getResponseBody resp
  case statusMaybe of
    Nothing -> fail "Invalid JSON (waitForReply)"
    Just (t :: Text) | t == "finished" -> do
      let resultParser = withObject "reply object" $ \o -> o .: "result"
          resultMaybe  = parseMaybe resultParser $ getResponseBody resp
      case resultMaybe of
        Just r -> do
          $(logInfo) $ "Received a complete result for query" <> (pack $ show qId)
          return (Right r)
        Nothing ->  fail $ "Failed to read JSON response to queryId " <> show qId
                     | t == "exception" -> return (Left "Exception" )
                     | otherwise -> do
                       liftIO $ threadDelay $ 1 * 1000 * 1000
                       waitForReply settings queryId
    
  where
    req = parseRequest_ (appReplyCacheURL ++ show qId)

resultWidget :: DataDictionary IO -> Text -> Either String ResultSetMsg -> Handler Widget
resultWidget dict normalizedText (Left reason) = do
  return $ [whamlet| <h1> Exception: #{reason}|]
resultWidget dict normalizedText (Right ResultSetMsg{..}) = do
  return $ do
    [whamlet|
<h1> Search Results:

<b>#{show nFragments} results were found! <br>

^{clusterSizeW} <br>
^{internalIdW} <br>

          |]
    resultsWithContext <- liftIO $ runMaybeT $
                            mapM (withContext  dict) resultSetResultList
    case resultsWithContext of
      Just rs -> mapM_ (resultMsg normalizedText)
                       (reverse $ sortOn (questionRating . snd) rs)
      Nothing -> return ()
  where
    clusterSizeW = [whamlet|Backend cluster size: #{resultClusterSize}|]
    internalIdW  = [whamlet|Internal search id:   #{show $ resultSetQueryId}|]
    nFragments = length resultSetResultList

withContext :: DataDictionary IO -> ResultMsg -> MaybeT IO (ResultMsg, Question)
withContext dict msg@ResultMsg{..} = do
  parentId <- Dict.answerParent dict aId
  parentQuestion <- Dict.getQuestion dict parentId
  return (msg, parentQuestion)
  where
    (aIdTxt, rest) = Text.span isDigit resultSource
    aIdInt = read $ Text.unpack aIdTxt
    fragIdInt = read $ Text.unpack $ Text.takeWhile isDigit $ Text.tail rest :: Int
    aId = AnswerId aIdInt

resultMsg :: Text -> (ResultMsg, Question) -> Widget
resultMsg code (ResultMsg{..}, q) = do
  detailId <- newIdent

  let parentText = questionTitle q
  
  [whamlet|
<h3> Question: #{parentText}
<a href=#{link}> Answer #{show aIdInt}, Fragment #{show fragIdInt}
<br>

<a .btn .btn-info data-toggle="collapse" data-target="##{detailId}">Details</button>

<div id="#{detailId}" class="collapse">
  ^{genDetails detailId}
<br>|]
  where
    (aIdTxt, rest) = Text.span isDigit resultSource
    aIdInt = read $ Text.unpack aIdTxt
    fragIdInt = read $ Text.unpack $ Text.takeWhile isDigit $ Text.tail rest :: Int
    aId = AnswerId aIdInt
    link = "http://stackoverflow.com/a/" ++ show aIdInt
    
    genDetails ident = do
      [whamlet|<h4> Alignment matches:|]
      mapM_ (alignmentMatchW code) resultAlignmentMatches

alignmentMatchW :: Text -> AlignmentMatchMsg -> Widget
alignmentMatchW code AlignmentMatchMsg{..} =
  [whamlet|<h5> Alignment match:

Levenshtein distance: #{show alignmentMatchLevenScore} <br>

Code in query document that was covered:
<div .well>
  <pre>
    <code>
      #{textInRange alignmentMatchResultTextRange code}
|]

submitCodeForm :: Html
               -> MForm Handler (FormResult (Text, Text, SearchSettings), Widget)
submitCodeForm extra = do
  (codeVal, codeView) <- (mreq textareaField " " Nothing)
  (sensVal, sensView) <- mreq sensitivityField "" (Just 0)
  (lenVal , lenView ) <- mreq sensitivityField" " (Just 20)
  (percVal, percView) <- mreq percField " " (Just 75.0)
  (displayVal, displayView) <- mreq checkBoxField " " (Just True)
  (blocksVal, blocksView) <- mreq checkBoxField " " (Just False)
  (idWordVal, idWordView) <- mreq checkBoxField " " (Just False)
  (idThreshVal, idThreshView) <- mreq percField " " (Just 25.0)
  (langVal, langView) <- mreq (selectFieldList [("Java" :: Text,"java" :: Text), ("Python3","python")]) " " (Just "java")
  
  let idFilter = case idWordVal of
        FormSuccess True ->
          case idThreshVal of
            FormSuccess n -> Just (n / 100.0)
            _ -> Nothing
        _ -> Nothing

      searchConfig = SearchSettings <$> lenVal
                                    <*> sensVal
                                    <*> ((/ 100.0) <$> percVal)
                                    <*> blocksVal
                                    <*> pure idFilter
                                    
      queryVal = (,,) <$> (unTextarea <$> codeVal) <*> langVal <*> searchConfig
      widget = do
        toWidget 
          [lucius|
               ##{fvId codeView} {
                 width: 100%;
                 height:30em;
               }

               td {
                 padding: 1em;
                  }
                  |]
        [whamlet|#{extra}
               <div class="well">
                 Your code: <br>
                 ^{fvInput codeView}
                 <table>
                   <tr>
                     <td>
                       Language: ^{fvInput langView}
                   <tr>
                     <td>
                       <h3> Alignment Settings
                       <table>
                         <tr>
                           <td>
                             Sensitivity: <br>
                             ^{fvInput sensView}
                           <td>
                             Minimal match length: <br>
                             ^{fvInput lenView}
                     <td>
                       <h3> Aggregation Settings
                       <table>
                         <tr>
                           <td>
                             Aggregation min %: <br>
                             ^{fvInput percView}
                   <tr>
                     <td>
                       <h3> Semantic Settings
                       <table>
                         <tr>
                           <td>
                             Block accordance filter: ^{fvInput blocksView}
                         <tr>
                           <td>
                             Identifier word similarity filter:
                             ^{fvInput idWordView}
                           <td>
                             Id similarity threshold:<br>
                             ^{fvInput idThreshView}
                     
                   <tr>
                     <td>
                       <h3> Display Settings
                       <table>
                         <tr>
                           <td>
                             Debug output: ^{fvInput displayView}
                 <br>
                 <br>
                 <center>
                   <button .btn .btn-primary .btn-large type=submit>
                     Submit!
                |]
  return (queryVal, widget)

  where
    langText = LanguageText . unTextarea
    sensitivityField = checkBool (>= 0)
                                 ("Sensitivity must be >= 0" :: Text)
                                 intField
    lenField = checkBool (> 0) ("Length must be greater > 0" :: Text) intField
    percField = checkBool (\x -> x >= 0.0 && x <= 100.0)
                          ("Give a percentage (0 - 100)" :: Text)
                          doubleField
