module Handler.Display where

import           Data.Char (isDigit)
import           Control.Monad.Trans.Maybe
import qualified Data.Text as Text
import           Helper
import           Import
import           Prelude (read)
import qualified Prelude as Prelude
import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question
import           Thesis.Data.Stackoverflow.Dictionary as Dict
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import Text.Julius (RawJS (..))
import Thesis.Util.LoggingUtils

getDisplayR :: Int -> Handler Html
getDisplayR qInt = do
  App{..} <- getYesod
  reply <- waitForReply appSettings queryId
  case reply of
    Right resultSet -> 
      defaultLayout $ do
        setTitle "CodeK\333an"
        $(widgetFile "display")
    Left errorMsg -> do
      defaultLayout $ [whamlet|
<h1>There was an error in the search backend!

Details: #{errorMsg} <br>
Query identifier: #{qInt}
|]
  where
    queryId = QueryId qInt

resultsW :: ResultSetMsg -> Widget
resultsW resultSet@ResultSetMsg{..} = do
  if null resultSetResultList
    then [whamlet|<h1>Your Search Yielded no Results
                 <br style="margin-bottom:10em">
                 |]
    else do
      [whamlet|<h1 style="text-align:center">Found #{length resultSetResultList} code pattern #{reuses}:
              |]
      forM_ (sortedResults resultSet) $ \result ->
        [whamlet|
                <div .row>
                  <div .col-lg-12>
                    ^{singleResultW resultSet result}
                |]
  where
    reuses :: Text
    reuses = if length resultSetResultList > 1
             then "reuses"
             else "reuse"

sortedResults :: ResultSetMsg -> [ResultMsg]
sortedResults ResultSetMsg{..} =
  reverse $ sortOn (resultCoverage resultQueryText) resultSetResultList

resultCoverage :: Text -> ResultMsg -> Double
resultCoverage t ResultMsg{..} = coveragePercentage (Text.length t) ranges
  where
    ranges = alignmentMatchPatternTextRange <$>resultAlignmentMatches

singleResultW :: ResultSetMsg -> ResultMsg -> Widget
singleResultW ResultSetMsg{..} ResultMsg{..} = do
  App{..} <- getYesod
  Just (parentQuestion, answer) <- liftIO $ runOutstreamLogging $ runMaybeT $ do
    parentId <- Dict.answerParent appDict aId
    q <- Dict.getQuestion appDict parentId
    a <- Dict.getAnswer appDict aId
    return (q,a)

  [whamlet|
          <div .row>
            <div .col-lg-12>
              <div .row>
                <div .col-lg-12>
                  <h3><a target="_blank" href=#{link}>#{questionTitle parentQuestion}</a><br>
                  <p>
                    The source answer is rated rating: #{answerRating answer}<br>
                    The answer parent's rating: #{questionRating parentQuestion}
                  <button type="button" class="btn btn-default" onclick="showDialog('#{resultSource}')">
                    <span class="glyphicon glyphicon-thumbs-up"></span>
                    Give Feedback!
              <div .row>
                <div .col-lg-12>

              <div .row>
                <div .col-lg-6>
                  The matched #{pieces} from your query document:
                  <br>
                  <br>
                  ^{queryPieces}
                <div .col-lg-6>
                  The matched code fragment:
                  <br>
                  <br>
                  <div .well style="overflow:scroll">
                    <pre>
                      
                        #{resultFragmentText}
          <hr>
          |]
  where
    link = "http://stackoverflow.com/a/" <> (Text.takeWhile isDigit resultSource)
    queryPieces = forM_ resultAlignmentMatches $ \AlignmentMatchMsg{..} -> do
      let qt = textInRangeNice alignmentMatchQueryTextRange resultQueryText
      [whamlet|
              <div .well>
                <pre>

                    #{qt}
              |]
    pieces :: Text
    pieces = if length resultAlignmentMatches > 1
             then "pieces"
             else "piece"

    (aIdTxt, rest) = Text.span isDigit resultSource
    aIdInt = read $ Text.unpack aIdTxt
    fragIdInt = read $ Text.unpack $ Text.takeWhile isDigit $ Text.tail rest :: Int
    aId = AnswerId aIdInt

textInRangeNice :: Range Text -> Text -> Text
textInRangeNice range text =
  let (txt, (start,end)) = textLinesInRange range text
      txtLines = zip formattedLineNumbers (Text.lines txt)
      formattedLineNumbers :: [Text]
      formattedLineNumbers =
        let nrs = take (end - start) $ Text.pack . show <$> [start ..]
            maxLength = Prelude.maximum $ (Text.length <$> nrs :: [Int])
        in fillTo maxLength <$> nrs
      
      txt' = Text.unlines $ do
        (nr, line) <- txtLines
        return $ nr <> line
  in txt'

fillTo :: Int -> Text -> Text
fillTo n t | Text.length t >= n = t <> ":"
           | otherwise =
               (Text.pack $ take (n - (Text.length t)) (repeat ' ')) <> t <> ":"
