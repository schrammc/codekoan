module Handler.Submit where

import Import

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List as List (intersperse, foldl1)

import Control.Monad.Trans.Maybe

import Text.Printf
import qualified Data.Text as Text

import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Java
import Thesis.CodeAnalysis.Language.Python
import Thesis.CodeAnalysis.Semantic.Blocks
import Thesis.CodeAnalysis.Semantic

import Thesis.Search
import Thesis.Search.ResultSet
import Thesis.Search.Settings

import Thesis.Data.Range
import Thesis.Data.Range (coveragePercentage)
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dictionary as Dict

import Thesis.Messaging.Query

--import Helpers
--import Handler.Display

import qualified Data.Vector as V


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
      let query = buildQuery code language settings

      return (Nothing :: Maybe Widget)
    _ -> return . Just $ [whamlet| <div .alert .alert-danger>
                                     <center> <b>
                                       Invalid input |]

  defaultLayout $ do
    setTitle "Code Foo"
    $(widgetFile "submit")

{-
postSubmitR :: Handler Html
postSubmitR = do
  ((formResult, formWidget), formEnctype) <- runFormPost submitCodeForm
  App{..} <- getYesod
  result <- case formResult of
    FormSuccess (txt, conf) -> liftIO $ runSearch (txt, conf)
    _ -> return $ Nothing
  defaultLayout $ do
    setTitle "Code Analysis"
    $(widgetFile "submit")

runSearch :: (LanguageText Java, SearchConfig)
          -> IO (Maybe (Maybe String, Widget))
runSearch (txt, conf@SearchConfig{..}) = do
  let tokens = processAndTokenize java txt
      widget = case tokens of
        Nothing -> defaultWidget
        Just tks -> buildResultWidget conf (normalize java txt) tks
  return (Just $ (show <$> tokens, widget))
  where
    defaultWidget = [whamlet|Tokenizer failure!|]

buildResultWidget :: SearchConfig -> LanguageText Java -> TokenVector Token Java -> Widget
buildResultWidget SearchConfig{..} txt tks = do
  App{..} <- getYesod
  searchResultWidgetMaybe <- liftIO $ runMaybeT $ do
    result <- MaybeT $ return $ answersWithCoverage aggregationPercentage
                                <$> fragmentsLongerThan minMatchLength
                                <$> findMatches appIndex levenSensitivity txt
    displayResult <- if blockAccordanceFilter
                     then resultSetBlockAnalysis appDict
                                                 java
                                                 result
                                                 javaBlockData
                                                 (token <$> tks)
                     else return result
    displayResult' <- if similarityFilter
                      then MaybeT $ ( Just <$>
                             answersWithCoverage aggregationPercentage <$>
                             (resultsWithSimilarity java
                                                   appDict
                                                   appSemantic
                                                   (tks, txt)
                                                   displayResult
                                                   similarityThreshold))
                      else return displayResult
    if debugOutput
      then do
        return $ resultWidget appDict txt displayResult'
      else undefined
  case searchResultWidgetMaybe of
    Nothing -> [whamlet|Search Failure!|]
    Just w  -> w
  

resultWidget :: Show t => DataDictionary IO
             -> LanguageText l
             -> ResultSet t l
             -> Widget
resultWidget dict txt resultSet = do
  [whamlet|<h2> SearchResults:|]
  summaryWidget resultSet
  sequence_ (answerGroupW dict txt <$> M.toList (resultSetMap resultSet))

answerGroupW :: Show t
                => DataDictionary IO
                -> (LanguageText l)
                -> (AnswerId, M.Map Int [[SearchResult t l]])
                -> Widget
answerGroupW dict txt (aId@AnswerId{..}, mp) = do
  [whamlet|<h3>Answer ^{linkToAnswer dict aId}:|]
  forM_ (M.toList mp) $ \(fragId, resultGroups) -> do
    case resultGroups of
      [] -> [whamlet|<h4>Fragment #{show fragId} has no results|]
      _ -> do
        [whamlet|<h4> Answer fragment #{show fragId}|]
        let groupListW = forM_ resultGroups $ \group ->
                          [whamlet| <li class="list-groupitem">
                                       ^{resultGroupWidget txt group}|]
        [whamlet|<ul class="list-group">
                   ^{groupListW}|]


-- | Build a nicely formatted output widget for a search result
resultGroupWidget :: Show t
                => (LanguageText l) -- ^ The query code to which the range pertains
             -> [SearchResult t l] -- ^ Search result
             -> Widget 
resultGroupWidget txt group = do
  [whamlet|<h4> Result group|]
  mapM_ (singleResultWidget txt) group

singleResultWidget :: Show t => (LanguageText l) -> SearchResult t l -> Widget
singleResultWidget txt res@SearchResult{..} = do
  dict <- appDict <$> getYesod
  [whamlet|
    Distance: #{resultLevenScore}; matched: #{fPerc} %
    <br>
    ^{codeSnippetWidget resultTextRange txt}
    <h5> Matched tokens:
    #{showList resultMatchedTokens} <br>
          |]
  where
    n = length resultMatchedTokens
    fPerc :: String
    fPerc = printf "%.2f" $ 100.0 *
                            (coveragePercentage (fragmentMetaSize resultMetaData)
                                                [resultFragmentRange])
    showList xs = "[" ++ (concat $ List.intersperse ", " $ show <$> xs)  ++ "]" :: String

codeSnippetWidget :: Range (LanguageText l) -> (LanguageText l) -> Widget
codeSnippetWidget range@(Range pa pb) LanguageText{..} =
  [whamlet|<div class="well">
             <code>#{codeText}|]
  where
    codeText = textInRange range langText

data SearchConfig = SearchConfig { levenSensitivity :: Int
                                 , minMatchLength :: Int
                                 , aggregationPercentage :: Double
                                 , debugOutput :: Bool
                                 , blockAccordanceFilter :: Bool
                                 , similarityFilter :: Bool
                                 , similarityThreshold :: Double
                                 }
-}

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
  
  let searchConfig = SearchSettings <$> lenVal
                                    <*> sensVal
                                    <*> ((/ 100.0) <$> percVal)
                                    <*> blocksVal
                                    <*> pure Nothing
                                    
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
