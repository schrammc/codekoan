{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Thesis.Survey.Mutation where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Random.Strict
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PSQL
import           Debug.Trace
import           Network.HTTP.Simple
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java
import           Thesis.CodeAnalysis.Language.Python
import           Thesis.CodeAnalysis.Mutation
import           Thesis.CodeAnalysis.Semantic
import           Thesis.CodeAnalysis.Semantic.MonadicAnalyzer
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Dump.Source.Postgres
import           Thesis.Messaging.SemanticQuery
import           Thesis.Search.Index
import           Thesis.Search.Settings (highSimilarityDef)
import           Thesis.Survey.LocalWithIndex (lowSimilarityDef, medSimilarityDef)

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy


defaultCi = PSQL.defaultConnectInfo
              {PSQL.connectHost = "damar.pms.ifi.lmu.de"
              , PSQL.connectPassword = "analytics-pgs"
              }

instance MonadRandom (LoggingT IO) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance MonadRandom (NoLoggingT IO) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms


mainMut :: IO ()
mainMut = do
  conn <- PSQL.connect defaultCi
  as <- answersWithTagsRandomOrder conn ["python"] 50000
  fragList <- fragmentsFromAnswers lang as

  let fragListFiltered =
        filter (\(md, txt ,_) ->
                  length (statementRanges lang txt) == minStatements &&
                   not (fragmentMetaAnswerId md `elem` exclusionList))
               fragList
      exclusionList = [AnswerId 769302, AnswerId 817004]
      stmtCorpus = buildStatementCorpus lang $ fmap (\(_,t,_) -> t) fragListFiltered
                 
  putStrLn $ "Found " ++ (show $ length fragListFiltered) ++
             " fragments with at least " ++ (show minStatements) ++
             " statements out of " ++ (show $ length fragList) ++ " fragments."

  let mutF = insertRandomStatementFromCorpus lang stmtCorpus
      resultsForSettings settings =  do
        forM (zip [1..] fragListFiltered) $ \(k,(md,txt,_)) -> runNoLoggingT $ do
          liftIO $ putStrLn $ show k ++ "\t" ++ (show md)
          line <- mutateSelfSearchNTimes mutF settings repeats txt
          liftIO $ print line
          and line `seq` return line
      trueFraction lst = (fromIntegral $ length $ filter id lst) / (fromIntegral $ length fragListFiltered) :: Double

  lowResults <- fmap ((fmap trueFraction) . List.transpose)
                $ resultsForSettings lowSimilarityDef
  putStrLn "low done"
  print lowResults

  medResults <- fmap ((fmap trueFraction) . List.transpose)
                $resultsForSettings medSimilarityDef
  putStrLn "med done"
  print medResults

  highResults <- fmap ((fmap trueFraction) . List.transpose)
                 $ resultsForSettings highSimilarityDef
  putStrLn "high done"
  print highResults

  let vals = zip (show <$> [1..])
                 (List.transpose [lowResults, medResults, highResults])

  chartToFile vals "Random Statement Insertion" "insertion_python.pdf"
                                
  return ()
  where
    lang = python
    minStatements = repeats
    repeats = 10
    mutNTimes mutFunction 0 txt = return txt
    mutNTimes mutFunction n txt = mutFunction txt >>= mutNTimes mutFunction (n-1)
    remoteAnalyzer = buildMonadicAnalyzer getSimilarity
    getSimilarity a b = do
      let submitR = setRequestMethod "POST" $
                    setRequestBodyJSON (SemanticQuery a b) $ 
                            parseRequest_ ("http://damar.pms.ifi.lmu.de:6366/submit")
      resp <- httpJSON submitR
      return $ getResponseBody resp

    mutateSelfSearchNTimes mutFunction settings n t = go 0 t
      where
        go k txt | k == n = return []
                 | k < n = do
          (found, modified) <- mutateOnceAndSearchAgainst lang
                                                          mutFunction
                                                          remoteAnalyzer
                                                          settings
                                                          txt
                                                          t
          rest <- (Text.length (langText modified)) `seq` go (k+1) modified
          return $ found:rest
           
chartToFile :: [(String, [Double])] -> String -> FilePath -> IO ()
chartToFile values title path = 
  toFile fo path $ do
    layout_title .= title
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
  where
    titles = ["Low","Medium","High"]
    fo = if List.isSuffixOf ".pdf" path
         then FileOptions (800, 600) PDF
         else def
    
