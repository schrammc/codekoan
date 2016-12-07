module Handler.Display where

import           Data.Char (isDigit)
import           Control.Monad.Trans.Maybe
import qualified Data.Text as Text
import           Helper
import           Import
import           Prelude (read)
import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question
import           Thesis.Data.Stackoverflow.Dictionary as Dict
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import Text.Julius (RawJS (..))

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
      [whamlet|<h1 style="text-align:center">Found #{length resultSetResultList} code pattern #{reuses}:
              |]
      forM_ resultSetResultList $ \result ->
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

singleResultW :: ResultSetMsg -> ResultMsg -> Widget
singleResultW ResultSetMsg{..} ResultMsg{..} = do
  App{..} <- getYesod
  Just parentQuestion <- liftIO $ runMaybeT $ do
    parentId <- Dict.answerParent appDict aId
    Dict.getQuestion appDict parentId
  [whamlet|
          <div .row>
            <div .col-lg-12>
              <div .row>
                <div .col-lg-12>
                  <h3><a href=#{link}>#{questionTitle parentQuestion}</a>
                  <button type="button" class="btn btn-default" onclick="showDialog('#{resultSource}')">
                    <span class="glyphicon glyphicon-thumbs-up"></span>
                    Give Feedback!
              <div .row>
                <div .col-lg-12>
                  <h4 style="text-align:center">Side-by-side comparison
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

    (aIdTxt, rest) = Text.span isDigit resultSource
    aIdInt = read $ Text.unpack aIdTxt
    fragIdInt = read $ Text.unpack $ Text.takeWhile isDigit $ Text.tail rest :: Int
    aId = AnswerId aIdInt

    --queryCode = textInRange alignmentMatchResultTextRange resultFragmentText
