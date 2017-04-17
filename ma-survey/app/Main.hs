{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Thesis.Messaging.Query
import Thesis.Messaging.ResultSet
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import           Control.Monad
import qualified Data.Yaml
import           System.Directory
import           System.Environment
import           Thesis.SurveySettings
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Thesis.Search.Settings
import Network.HTTP.Conduit
import Network.HTTP.Simple hiding (httpLbs)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import Network.Connection
import Data.Maybe (catMaybes)
main = do
  args <- getArgs
  case args of
    settingsPath:dirPath:[] -> do
      settingsMaybe <- Data.Yaml.decodeFile settingsPath :: IO (Maybe SurveySettings)
      case settingsMaybe of
        Nothing -> do
          putStrLn "Settings file doesn't contain valid yaml!"
        Just settings -> runWithSettings settings dirPath
    _ -> printHelpString

printHelpString = do
  putStrLn "usage: <settings-path> <directory-path>"

runWithSettings :: SurveySettings -> FilePath -> IO ()
runWithSettings settings dirPath = do
  files <- filterCodeFiles <$> allFiles dirPath
  when (null files) $
    print $ "Warning: no files to process in directory " ++ dirPath
  results <- mapConcurrently (process settings) files
  let fileText = T.decodeUtf8 . BS.concat . BL.toChunks . Aeson.encode $ results
  TIO.writeFile "output.json" fileText
  return ()
  where
    process settings path = do
      res <- processFile settings path
      return (path, res)

filterCodeFiles :: [FilePath] -> [FilePath]
filterCodeFiles ps =
  filter (\p -> isSuffixOf ".java" p ||
                isSuffixOf ".py" p ||
                isSuffixOf ".hs" p
         ) ps

processFile :: SurveySettings -> FilePath -> IO (Maybe ResultSetMsg)
processFile settings path = do
  print $ "Processing: " ++ path
  queryId <- submitFile settings path
  print $ "Query id for " ++ path ++ " " ++ (show queryId)
  result <- waitForResult settings queryId
  print $ "Finished processing file " ++ path
  return result

submitFile :: SurveySettings -> FilePath -> IO QueryId
submitFile surveySettings path = do
  query <- buildQuery
  initialReq <- parseRequest . T.unpack $ submitUrl surveySettings
  let req = setRequestMethod "POST" $
            setRequestBodyJSON query initialReq
  response <- httpJSON req
  let maybeQId=  parseQueryIdFromResponse (getResponseBody response)
  case maybeQId of
    Nothing -> error "Failed to parse queryId!"
    Just qId -> return qId
  where
    parseQueryIdFromResponse :: Aeson.Value -> Maybe QueryId
    parseQueryIdFromResponse res =
      let parser = Aeson.withObject "person" $ \o -> o .: "queryId"
      in Aeson.parseMaybe parser res
    derivedLang | isSuffixOf ".hs" path = "haskell"
                | isSuffixOf ".py" path = "python"
                | otherwise             = "java"
    settings = SearchSettings { minMatchLength = 10
                              , levenshteinDistance = 0
                              , coveragePercentage = 0.7
                              , blockFiltering = True
                              , semanticThreshold = Just 0.3
                              }
    buildQuery = do
      t <- TIO.readFile path
      return $ Query { queryText = t
                     , queryLanguage = T.pack derivedLang
                     , queryId = Nothing
                     , querySettings = settings
                     }

data ResultStatus = StatusPending
                  | StatusFinished
                  | StatusException

waitForResult :: SurveySettings -> QueryId -> IO (Maybe ResultSetMsg)
waitForResult settings (QueryId qId) = do
  threadDelay sleepTime
  go
  where
    go = do
      result <- tryGet
      case result of
        Right (r, StatusFinished) -> return $ r
        Right (_, StatusException) -> return Nothing
        _ -> threadDelay sleepTime >> go
    tryGet = do
      req <- parseRequest $ T.unpack (getUrl settings) ++ "/" ++ show qId
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- httpLbs req manager
      let Just resp = Aeson.decode (getResponseBody res)
      case resp of
        Nothing -> return $ Left "Json parse failure"
        Just responseJSON -> case Aeson.parseMaybe parser responseJSON of
          Nothing -> return $ Left "Json parse failure (structural)"
          Just r -> return $ Right r
      
    parser = Aeson.withObject "object" $ \o -> do
      status <- o .: "status"
      if | status == ("finished" :: String) -> do
           res <- o .: "result" :: Aeson.Parser ResultSetMsg
           return (Just res, StatusFinished)
         | status == "pending" ->
           return (Nothing, StatusPending)
         | otherwise -> fail ""
    sleepTime = 10 * 1000 * 1000

allFiles :: FilePath -> IO [FilePath]
allFiles path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then return (path:[])
    else do
      entries <- listDirectory path
      paths <- concat <$> forM ((\p -> path ++ "/" ++ p) <$>  entries) allFiles
      return $ paths
