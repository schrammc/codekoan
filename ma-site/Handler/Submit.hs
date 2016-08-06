module Handler.Submit where

import Import

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List as List (intersperse, foldl1)

import Text.Printf
import qualified Data.Text as Text

import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Java (Java, java)

import Thesis.Search
import Thesis.Search.ResultSet
import Thesis.Search.SearchResult

import Thesis.Data.Range
import Thesis.Data.Range (coveragePercentage)
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dictionary as Dict

import Helpers
import Handler.Display

getSubmitR :: Handler Html
getSubmitR = do
  (formWidget, formEnctype) <- generateFormPost submitCodeForm
  let result = Nothing :: Maybe (Maybe String, Maybe Widget)
  defaultLayout $ do
    setTitle "Code Analysis"
    $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
  ((formResult, formWidget), formEnctype) <- runFormPost submitCodeForm
  foundation <- getYesod
  let index = appIndex foundation
  let result =
        case formResult of
          FormSuccess (txt, sim, len, thresh, debugDisplay) ->
            let tks = processAndTokenize java txt
                searchResult = fragmentsLongerThan len <$>
                  answersWithCoverage thresh <$> findMatches index sim txt
                txt' = normalize java txt
                dict = appDict foundation
                resultW = if debugDisplay
                          then resultWidget dict (langText txt') <$> searchResult
                          else codeResultWidget dict (langText txt') <$> searchResult
            in Just (show <$> tks, resultW)
          _ -> Nothing
  defaultLayout $ do
    setTitle "Code Analysis"
    $(widgetFile "submit")

resultWidget :: Show t => DataDictionary -> Text  -> ResultSet t -> Widget
resultWidget dict txt (ResultSet{..}) = do
  [whamlet|<h2> SearchResults:|]
  sequence_ (answerGroupW dict txt <$> M.toList resultSetMap)

answerGroupW :: Show t
                => DataDictionary
                -> Text
                -> (AnswerId, M.Map Int [SearchResult t])
                -> Widget
answerGroupW dict txt (aId@AnswerId{..}, mp) = do
  [whamlet|<h3>Answer ^{linkToAnswer dict aId}:|]
  forM_ (M.toList mp) $ \(fragId, results) -> do
    
    case results of
          [] -> [whamlet|<h4>Fragment #{show fragId} has no results|]
          (r:rs) -> let n = fragmentMetaSize $ resultMetaData r
                        fragRanges = resultFragmentRange <$> results
                        percentage = 100.0 * coveragePercentage n fragRanges
                        p = printf "%.2f" percentage :: String
                    in do
                      [whamlet|<h4>Fragment #{show fragId} (coverage: #{p}%)|]
                      forM_ results (singleResultWidget txt)


-- | Build a nicely formatted output widget for a search result
singleResultWidget :: Show t
                => Text -- ^ The query code to which the range pertains
             -> SearchResult t -- ^ Search result
             -> Widget 
singleResultWidget txt res@(SearchResult{..}) = do
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
    fPerc = printf "%.2f" $
            100.0 * ((fromIntegral n :: Double) /
                     (fromIntegral $ fragmentMetaSize resultMetaData))
    showList xs = "[" ++ (concat $ List.intersperse ", " $ show <$> xs)  ++ "]" :: String

codeSnippetWidget :: Range -> Text -> Widget
codeSnippetWidget range@(Range pa pb) t =
  [whamlet|<div class="well">
             <code>#{codeText}|]
  where
    codeText = textInRange range t

submitCodeForm :: Html
               -> MForm Handler (FormResult (LanguageText Java, Int, Int, Double, Bool), Widget)
submitCodeForm extra = do
  (codeVal, codeView) <- (mreq textareaField "lfoo" Nothing)
  (sensVal, sensView) <- mreq sensitivityField "" (Just 0)
  (lenVal , lenView ) <- mreq sensitivityField" " (Just 20)
  (percVal, percView) <- mreq percField " " (Just 75.0)
  (displayVal, displayView) <- mreq checkBoxField " " (Just False)

  ngramSize <- appNGramSize <$> getYesod
  
  let queryVal = (,,,,) <$> (langText <$> codeVal)
                       <*> sensVal
                       <*> lenVal
                       <*> ((/ 100.0) <$> percVal)
                       <*> displayVal
      widget = do
        toWidget 
          [lucius|
               ##{fvId codeView} {
                 width: 80em;
                 height:30em;
               }

               td {
                 padding: 1em;
                  }
                  |]
        [whamlet|#{extra}

                 <table>
                   <tr>
                     <td.codeInput>
                       Your java code: <br>
                       ^{fvInput codeView}
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
                             NGram Size is #{ngramSize}
                   <tr>
                     <td>
                       <h3> Aggregation Settings
                       <table>
                         <tr>
                           <td>
                             Aggregation min %: <br>
                             ^{fvInput percView}
                   <tr>
                     <td>
                       <h3> Display Settings
                       <table>
                         <tr>
                           <td>
                             Debug output: ^{fvInput displayView}
                 <br>
                 <br>
                 <button type=submit>
                   Submit my code
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
