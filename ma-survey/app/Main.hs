{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Foldable (foldl')
import           Data.List (isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml
import qualified Database.PostgreSQL.Simple as PSQL
import           Network.Connection
import           Network.HTTP.Conduit
import           Network.HTTP.Simple hiding (httpLbs)
import           Statistics.Distribution
import           Statistics.Distribution.Normal
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Thesis.Data.Range as Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Dictionary.Postgres
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import           Thesis.Search.Settings
import           Thesis.SurveySettings
import           Thesis.Util.MonadUtils

main = do
  args <- getArgs
  case args of
    "process":settingsPath:dirPath:[] -> do
      settingsMaybe <- Data.Yaml.decodeFile settingsPath :: IO (Maybe SurveySettings)
      case settingsMaybe of
        Nothing -> do
          putStrLn "Settings file doesn't contain valid yaml!"
        Just settings -> runWithSettings settings dirPath
    "analyze":settingsPath:filePath:[] -> do
      settingsMaybe <- Data.Yaml.decodeFile settingsPath :: IO (Maybe SurveySettings)
      case settingsMaybe of
        Nothing -> do
          putStrLn "Settings file doesn't contain valid yaml!"
        Just settings -> do
          putStrLn $ "Analyzing " ++ filePath
          runAnalysis settings filePath
      return ()
    _ -> printHelpString

printHelpString = do
  putStrLn "usage: <settings-path> <directory-path>"

runWithSettings :: SurveySettings -> FilePath -> IO ()
runWithSettings settings dirPath = do
  files <- filterCodeFiles <$> allFiles dirPath
  when (null files) $
    putStrLn $ "Warning: no files to process in directory " ++ dirPath
  results <- mapM (process settings) files
  let fileText = T.decodeUtf8 . BS.concat . BL.toChunks . Aeson.encode $ results
  TIO.writeFile (directoryName dirPath ++".json") fileText
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
  putStrLn $ "Processing: " ++ path
  queryId <- submitFile settings path
  putStrLn $ "Query id for " ++ path ++ " " ++ (show queryId)
  result <- waitForResult settings queryId
  putStrLn $ "Finished processing file " ++ path
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
        Right (_, StatusPending) -> threadDelay sleepTime >> go
        _ -> return Nothing
    tryGet = do
      req <- parseRequest $ T.unpack (getUrl settings) ++ "/" ++ show qId
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- httpLbs req manager
      let resp = Aeson.decode (getResponseBody res)
      case resp of
        Nothing -> return $ Left "Json parse failure"
        Just responseJSON ->
          case Aeson.parseMaybe parser responseJSON of
            Nothing -> return $ Left "Json parse failure (structural)"
            Just r -> return $ Right r
      
    parser = Aeson.withObject "object" $ \o -> do
      status <- o .: "status"
      if | status == ("finished" :: String) -> do
             res <- o .: "result" :: Aeson.Parser ResultSetMsg
             return (Just res, StatusFinished)
         | status == "pending" -> do
             res <- o .: "result" :: Aeson.Parser ResultSetMsg
             return (Just res, StatusPending)
         | status == "nothing" -> return (Nothing, StatusPending)
         | otherwise -> return (Nothing, StatusException)
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

runAnalysis :: SurveySettings -> FilePath -> IO ()
runAnalysis settings path = runStdoutLoggingT $ do
  resultMaybe <- liftIO $ Aeson.decode <$> BL.readFile path :: LoggingT IO (Maybe [(FilePath, Maybe ResultSetMsg)])
  case filterRelevant <$> resultMaybe of
    Nothing -> liftIO $ putStrLn "Parser failure"
    Just result -> do
      liftIO $ putStrLn $ "Parser success: " ++ (show $ length result)
      conn <- liftIO . PSQL.connect $ buildConnectInfo settings
      let justResults = catMaybes $ (\(a,b) -> (a,) <$> b) <$> result
      analyzeFreqs path settings conn justResults

      analyzeTags path settings conn  (snd <$> justResults)
      analyzeVerbatim path settings conn justResults
      
      return ()

filterRelevant :: [(FilePath, Maybe ResultSetMsg)]
               -> [(FilePath, Maybe ResultSetMsg)]
filterRelevant rs = do
  (path, maybeResult) <- rs
  return $ case maybeResult of
             Nothing -> (path, Nothing)
             Just res ->
               let resultList' = filter isRelevant (resultSetResultList res)
               in (path, Just $ res{resultSetResultList = resultList'})

analyzeFreqs :: MonadIO m =>
                FilePath
             -> SurveySettings
             -> PSQL.Connection
             -> [(FilePath, ResultSetMsg)]
             -> m ()
analyzeFreqs path settings conn resultSets = do
  liftIO $ writeFile (path ++ ".freqs") (unlines fileLines)
  liftIO $ writeFile (path ++ ".freqs.summary") $
    "Average coverage: " ++ (show $ sum coverages / sum relativeLengths)
  where
    (paths, sets) = unzip resultSets
    coverages = zipWith (*) (resCoverage <$> sets) relativeLengths
    lengths = T.length . resultQueryText <$> sets
    maxlength = fromIntegral $ maximum lengths
    relativeLengths =
      (flip fmap) lengths $ \l -> (fromIntegral l) / maxlength
    withPath = zip paths coverages
    sorted = reverse $ sortOn snd withPath
    fileLines = (flip fmap) sorted $ \(path, cov) -> path ++ ";" ++ (show cov)

analyzeVerbatim :: MonadIO m =>
                   FilePath
                -> SurveySettings
                -> PSQL.Connection
                -> [(FilePath, ResultSetMsg)]
                -> m ()
analyzeVerbatim prefix settings conn resultSets =
  liftIO $ writeFile (prefix ++ ".verbatimcounts") $ unlines resultLines
  where
    resultLines = (flip fmap) (sortOn (length . snd) results) $ \(path, rs) ->
      path ++ " -> " ++ (show $ length rs)
    results = do
      (path, resultSet@ResultSetMsg{..}) <- resultSets
      let pathResults = do
            result <- resultSetResultList
            if isCopy result resultQueryText
              then return result
              else []
      return (path, pathResults)
    isCopy ResultMsg{..} queryText = and $ do
      match <- resultAlignmentMatches
      return $ isCopyMatch match resultFragmentText queryText
    isCopyMatch AlignmentMatchMsg{..} patternText queryText =
      T.words (Range.textInRange alignmentMatchQueryTextRange queryText) ==
      T.words (Range.textInRange alignmentMatchPatternTextRange patternText)

resCoverage :: ResultSetMsg -> Double
resCoverage msg =
  let relevantResults = resultSetResultList msg
      matches = concat $ resultAlignmentMatches <$> relevantResults
      ranges = alignmentMatchQueryTextRange <$> matches
  in Range.coveragePercentage (T.length $ resultQueryText msg) ranges

analyzeTags :: MonadIO m =>
               FilePath
            -> SurveySettings
            -> PSQL.Connection
            -> [ResultSetMsg]
            -> m ()
analyzeTags path settings conn resultSets = do
  let results = concat $ resultSetResultList <$> resultSets
      sources = catMaybes $ sourceToFragId . resultSource <$> results

  liftIO $ putStrLn $ "Number of sources: " ++ (show $ length sources)

  let sourceCounts = M.toList $
                     M.unionsWith (+) $
                     (\aId -> M.singleton aId 1) <$> sources
  rootTagsMaybe <- runMaybeT . catMaybeTs $
                     (\(a,n) -> (,n) <$> postgresAnswerRootTags conn (fragmentAnswerId a)) <$> sourceCounts
  case rootTagsMaybe of
    Nothing -> error "unlikely case (shouldn't happen)"
    Just tagSets  -> do
      baseCounts <- M.filter (> 100) <$> (liftIO $ tagCountsForLanguage conn (T.pack "java"))
      let baseFreqs = relativeFrequencies baseCounts
          tagMaps = do
            (tagSet, count) <- tagSets
            return (M.fromSet (const count) tagSet)
          sampleCounts = M.unionsWith (+) tagMaps

          sampleFreqs = relativeFrequencies sampleCounts
          relativeReps = relativeRep baseFreqs sampleFreqs
          normals      = normalScores baseFreqs sampleCounts
          
          tm = reverse $ sortOn (snd) $ M.toList $ buildScores $
                 M.intersectionWith ProbAndCount normals sampleCounts

      let fileLines  = (flip fmap) tm $ \(tag, score) ->
            (T.unpack tag) ++ " -> " ++ (show score)
          pureLines = (flip fmap) (M.toList normals) $  \(tag, score) ->
            (T.unpack tag) ++ " -> " ++ (show score)
          countLines = (flip fmap) (M.toList sampleCounts) $ \(tag, count) ->
            (T.unpack tag) ++ " -> " ++ (show count)
      liftIO $ writeFile (path ++ ".tags") (unlines fileLines)
      liftIO $ writeFile (path ++ ".tags.pure") (unlines pureLines)
      liftIO $ writeFile (path ++ ".counts") (unlines countLines)

  return ()

buildScores :: M.Map T.Text ProbAndCount -> M.Map T.Text Double
buildScores mp = M.intersectionWith (+) adjustedFreqs (getProb <$> mp)
  where
    adjustedFreqs = freqToDouble . (\x -> 0.002 * x) <$> freqs
    freqs = relativeFrequencies $ getCount <$> mp

data ProbAndCount = ProbAndCount { getProb :: Double
                                 , getCount :: Int
                                 }

sourceToFragId :: T.Text -> Maybe AnswerFragmentId
sourceToFragId t =
  case AP.parseOnly parser t of
    Left _ -> Nothing
    Right x -> Just x
  where
    parser = do
      aIdInt <- AP.decimal
      AP.takeWhile (not . isDigit)
      fragIdInt <- AP.decimal
      return $ AnswerFragmentId (AnswerId aIdInt) fragIdInt

tagCountsForLanguage :: PSQL.Connection -> T.Text -> IO (M.Map T.Text Int)
tagCountsForLanguage conn t =
  PSQL.fold conn
            "SELECT tags FROM questions \
            \WHERE ? = ANY(questions.tags)"
            (PSQL.Only t)
            M.empty
            f
  where
    f :: M.Map T.Text Int -> PSQL.Only (V.Vector T.Text) -> IO (M.Map T.Text Int)
    f mp (PSQL.Only nexts) = return $!
      foldl' (\mp next -> M.insertWith (+) next 1 mp) mp nexts

newtype RelativeFrequency = RelativeFrequency {freqToDouble :: Double}
                          deriving ( Eq, Ord, Num, Show, RealFloat, RealFrac
                                   , Floating, Real, Fractional)
    
relativeFrequencies :: M.Map a Int -> M.Map a RelativeFrequency
relativeFrequencies mp = f <$> mp
  where
    maxValue = fromIntegral $ maximum mp
    f x = (fromIntegral x) / maxValue

-- | Find out which values are relatively overreprresented with respect to a
-- base popoulation. Values > 1 mean overrepresentation while values < 1 mean
-- underrepresentation.
relativeRep :: (Ord a) =>
               M.Map a RelativeFrequency
               -- ^ The totality
            -> M.Map a RelativeFrequency
            -- ^ The sample
            -> M.Map a RelativeFrequency
relativeRep totality sample = M.intersectionWith (*) sample totality

normalScores :: (Ord a) =>
                M.Map a RelativeFrequency
             -> M.Map a Int
             -> M.Map a Double
normalScores freqs counts = M.intersectionWith f freqs counts
  where
    n = maximum counts
    f frequency count =
      let freq = freqToDouble frequency
          sd = (fromIntegral n * freq * (1-freq))
          dist = normalDistr ((fromIntegral n) * freq)   sd
      in if freq == 1
         then 1
         else cumulative dist (fromIntegral count)

isRelevant :: ResultMsg  -> Bool
isRelevant msg =
  let totalTokenLength = sum $
        (Range.rangeLength . alignmentMatchPatternTokenRange) <$>
           (resultAlignmentMatches msg)
  in totalTokenLength > 30

fac n | n <= 0 = 0
      | otherwise = fac' n 1
  where
    fac' 1 !acc = acc
    fac' n !acc = fac' (n-1) (acc * n)

directoryName :: FilePath -> String
directoryName "" = ""
directoryName path =
  if fileName /= ""
  then fileName
  else takeFileName $ takeDirectory path
  where
    fileName = takeFileName path

average :: (Foldable f) => f Double -> Double
average ds = sum ds / (fromIntegral $ length ds)
