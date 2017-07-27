{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.Survey.LocalWithIndex where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Database.PostgreSQL.Simple as PSQL
import           Network.HTTP.Simple
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java
import           Thesis.CodeAnalysis.Semantic
import           Thesis.CodeAnalysis.Semantic.MonadicAnalyzer
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dictionary
import           Thesis.Data.Stackoverflow.Dictionary.Postgres
import           Thesis.Data.Stackoverflow.Dump.Source.Postgres
import           Thesis.Messaging.SemanticQuery
import           Thesis.Search
import           Thesis.Search.Settings
import           Thesis.Survey.DirHelper
import           Thesis.Util.ConduitUtils
import Thesis.Util.LoggingUtils
import Thesis.Search.ResultSet
import System.Environment

medSimilarityDef :: SearchSettings
medSimilarityDef = SearchSettings { minMatchLength = 10
                                  , levenshteinDistance = 0
                                  , coveragePercentage = 0.7
                                  , blockFiltering = True
                                  , semanticThreshold = Just 0.4
                                  , minSumResultLength = 20
                                  }

lowSimilarityDef :: SearchSettings
lowSimilarityDef = SearchSettings { minMatchLength = 10
                                  , levenshteinDistance = 0
                                  , coveragePercentage = 0.5
                                  , blockFiltering = True
                                  , semanticThreshold = Nothing
                                  , minSumResultLength = 20
                                  }

psqlConnectInfo =
  PSQL.defaultConnectInfo
    { PSQL.connectHost = "damar.pms.ifi.lmu.de"
    , PSQL.connectPassword="analytics-pgs"
    }

semanticURL = "http://damar.pms.ifi.lmu.de:6366/submit"

mainLocal :: IO ()
mainLocal = do
  dirPath:[] <- getArgs
  conn <- PSQL.connect psqlConnectInfo
  let tags = ["java", "postgresql"]
  dict <- runOutstreamLogging $ postgresDictionary psqlConnectInfo
  let getTokenV = \AnswerFragmentMetaData{..} -> MaybeT $ do
        fragMaybe <- runMaybeT $ getAnswerFragment dict
                                                   java
                                                   fragmentMetaId
        case fragMaybe of
          Nothing -> do
            $(logError) $ "Failed to get answer fragment " <>
                          (Text.pack $ show fragmentMetaId)
            return Nothing
          Just frag -> return $ Just frag

  expectedN <- answerWithTagsCount conn tags
  putStrLn $ "Expected number: " ++ show expectedN

  let answerSource = answersWithTags conn ["java", "postgresql"] =$=
                     everyN 500 (liftIO . print)

  index <- runStdoutLoggingT $ buildIndexForJava answerSource 10

  fs <- allCodeFiles dirPath

  total <- forM fs $ \path -> runOutstreamLogging $ do
    $(logInfo) $ "Working with file " <> (Text.pack path)
    content <- liftIO $ Text.readFile path
    res <- tokenizeAndPerformCachedSearch index
                                   java
                                   getTokenV
                                   medSimilarityDef
                                   (LanguageText content)
                                   remoteAnalyzer
    case res of
      Nothing -> return 0
      Just x  -> return $ numberOfFragments x
  print total
  putStrLn $ "Total: " ++ (show $ sum total) ++ " results in " ++ (show $ length fs) ++ " files."
  PSQL.close conn
  return ()

remoteAnalyzer = buildMonadicAnalyzer getSimilarity
  where
    getSimilarity a b = do
      let submitR = setRequestMethod "POST" $
                    setRequestBodyJSON (SemanticQuery a b) $ 
                            parseRequest_ (semanticURL)
      resp <- httpJSON submitR
      return $ getResponseBody resp
