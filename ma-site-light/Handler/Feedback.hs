{-# LANGUAGE RecordWildCards #-}
module Handler.Feedback where

import Import
import Thesis.Messaging.Feedback
import Thesis.Messaging.Query
import Helper
import Data.Aeson (toJSON)
import Text.Julius (RawJS (..))
import qualified Database.PostgreSQL.Simple as PSQL

postFeedbackR :: Handler Value
postFeedbackR = do
  App{..} <- getYesod
  feedback <- requireJsonBody :: Handler Feedback
  dbConnection <- liftIO $ PSQL.connect (appPostgresConnectInfo appSettings)
  liftIO $ indexFeedback dbConnection feedback
  return $ object ["status" .= ("accepted" :: Text)]

indexFeedback :: PSQL.Connection -> Feedback -> IO ()
indexFeedback dbConnection Feedback{..} = do
  now <- getCurrentTime
  let QueryId q = queryId
  PSQL.execute dbConnection "INSERT INTO feedback(query_id, sim, reuse, comment, source, language, time) VALUES (?, ?, ?, ?, ?, ?, ?)" (q, printYesNo similar, printYesNo reuse, comment, source, language, now)
  return ()

