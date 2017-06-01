module Thesis.Util.Benchmark.Subsumption where

import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Thesis.Data.Range
import Thesis.Search.AlignmentMatch
import qualified Data.Sequence as Seq
generateAlignmentMatches :: MonadIO m => Int -> m [AlignmentMatch Char () ()]
generateAlignmentMatches n = replicateM n generateAlignmentMatch

generateAlignmentMatch :: MonadIO m => m (AlignmentMatch Char () ())
generateAlignmentMatch = do
  tr <- randomRange $ Range 0 2000
  queryRange <- randomRange $ Range 0 2000
  fragRange <- randomRange $ Range 0 2000
  return $ AlignmentMatch tr Seq.empty queryRange () fragRange 0

randomRange :: MonadIO m => Range a  -> m (Range a)
randomRange (Range a b) = do
  x <- liftIO $ randomRIO (a,b)
  y <- liftIO $ randomRIO (a,b)
  return $ Range (min x y) (max x y)
