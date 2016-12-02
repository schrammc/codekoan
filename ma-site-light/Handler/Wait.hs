module Handler.Wait where

import Import
import Thesis.Messaging.Query
import Helper
import Data.Aeson (toJSON)
import Text.Julius (RawJS (..))

getWaitR :: Int -> Handler Html
getWaitR qInt = do
  defaultLayout $ do
    setTitle "CodeK\333an"
    $(widgetFile "wait")
  where
    queryId = QueryId qInt
