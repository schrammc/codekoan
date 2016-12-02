{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helper where

import Import 
import Network.HTTP.Simple
import Thesis.Messaging.Query
import Thesis.Messaging.ResultSet
import Data.Aeson
import Data.Aeson.Types (parseMaybe)

-- | Wait until there is a reply for the given query id in the reply-cache
waitForReply :: (MonadIO m, MonadLogger m) =>
                AppSettings
             -> QueryId
             -> m (Either String ResultSetMsg)
waitForReply settings@AppSettings{..} queryId@(QueryId qId) = do
  resp <- httpJSON req 
  
  let statusParser = withObject "reply object" $ \o -> o .: "status"
      statusMaybe  = parseMaybe statusParser $ getResponseBody resp
      resultParser = withObject "reply object" $ \o -> o .: "result"
      resultMaybe :: forall a . FromJSON a => Maybe a
      resultMaybe  = parseMaybe resultParser $ getResponseBody resp
  case statusMaybe of
    Nothing -> fail "Invalid JSON (waitForReply)"
    Just (t :: Text)
      | t == "finished" ->
        case resultMaybe of
          Just r -> do
            $(logInfo) $ "Received a complete result for query" <> (pack $ show qId)
            return (Right r)
          Nothing ->  fail $ "Failed to read JSON response to queryId " <> show qId
      | t == "exception" ->
        case resultMaybe of
          Just reason -> return (Left reason)
          Nothing -> return (Left "Unknown exception")
      | otherwise -> do
          liftIO $ threadDelay $ 1 * 1000 * 1000
          waitForReply settings queryId
    
  where
    req = parseRequest_ (appReplyCacheURL ++ show qId)
