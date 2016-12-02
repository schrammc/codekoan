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
    then [whamlet|<h1>Your Search Yielded no Results|]
    else do
      [whamlet|<h1>Reuse of  #{length resultSetResultList} code patterns: |]
      forM_ resultSetResultList $ \result ->
        [whamlet|
                <div .row>
                  <div .col-lg-12>
                    ^{singleResultW resultSet result}
                |]

singleResultW :: ResultSetMsg -> ResultMsg -> Widget
singleResultW ResultSetMsg{..} ResultMsg{..} =
  [whamlet|
          <a href=#{link}>#{resultSource}
          <div .well>
            <pre>
              #{resultFragmentText}
          <hr>
          |]
  where
    link = "http://stackoverflow.com/a/" <> (Text.takeWhile isDigit resultSource)
    --queryCode = textInRange alignmentMatchResultTextRange resultFragmentText

