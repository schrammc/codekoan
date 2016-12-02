module Handler.Display where

import Import
import Thesis.Messaging.Query
import Helper

getDisplayR :: Int -> Handler Html
getDisplayR qInt = do
  App{..} <- getYesod
  waitForReply appSettings queryId
  defaultLayout $ do
    setTitle "CodeK\333an"
    [whamlet|
            <img id="headerimage" src="/static/img/codekoan_short_logo.svg" class="float img-responsive">
            <hr>
            |]
  where
    queryId = QueryId qInt

