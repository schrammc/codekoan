module Handler.Display where

import Import
import Thesis.Messaging.Query
import Thesis.Messaging.ResultSet
import Thesis.Data.Range
import Helper
import qualified Data.Text as Text
import Data.Char (isDigit)

getDisplayR :: Int -> Handler Html
getDisplayR qInt = do
  App{..} <- getYesod
  Right resultSet <- waitForReply appSettings queryId
  defaultLayout $ do
    setTitle "CodeK\333an"
    $(widgetFile "display")
  where
    queryId = QueryId qInt

resultsW :: ResultSetMsg -> Widget
resultsW resultSet@ResultSetMsg{..} = do
  if null resultSetResultList
    then [whamlet|<h1>Your Search Yielded no Results
                 <br style="margin-bottom:10em">
                 |]
    else do
      [whamlet|<h1>I found #{length resultSetResultList} reuses of code patterns: |]
      forM_ resultSetResultList $ \result ->
        [whamlet|
                <div .row>
                  <div .col-lg-12>
                    ^{singleResultW resultSet result}
                |]

singleResultW :: ResultSetMsg -> ResultMsg -> Widget
singleResultW ResultSetMsg{..} ResultMsg{..} =
  [whamlet|
          <div .row>
            <div .col-lg-12>
              <div .row>
                <div .col-lg-12>
                  <a href=#{link}>#{resultSource}
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
                  <div .well>
                    <pre>
                      #{resultFragmentText}
          <hr>
          |]
  where
    link = "http://stackoverflow.com/a/" <> (Text.takeWhile isDigit resultSource)
    queryPieces = forM_ resultAlignmentMatches $ \AlignmentMatchMsg{..} -> do
      let qt = textInRange alignmentMatchResultTextRange resultQueryText
      [whamlet|
              <div .well>
                <pre>
                  #{qt}
              |]
    pieces :: Text
    pieces = if length resultAlignmentMatches > 1
             then "pieces"
             else "piece"

    --queryCode = textInRange alignmentMatchResultTextRange resultFragmentText
