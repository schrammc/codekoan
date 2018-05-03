module Thesis.Util.Benchmark.Subsumption where

import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Thesis.Data.Range
import Thesis.Search.AlignmentMatch
import qualified Data.Sequence as Seq

buildChunks _ [] = []
buildChunks n xs = let a = take n xs
                       b = drop n xs
                   in (Seq.fromList a):(buildChunks n b)

generateAlignmentMatches :: MonadIO m => Int -> m [AlignmentMatch Char () Int]
generateAlignmentMatches n = replicateM n $ liftIO $ do
  x <- randomRIO (0, 1000)
  generateAlignmentMatch x

generateAlignmentMatch :: MonadIO m => t -> m (AlignmentMatch Char () t)
generateAlignmentMatch x = do
  tr <- randomRange $ Range 0 2000
  queryRange <- randomRange $ Range 0 2000
  fragRange <- randomRange $ Range 0 2000
  return $ AlignmentMatch tr queryRange x fragRange 0

randomRange :: MonadIO m => Range a  -> m (Range a)
randomRange (Range a b) = do
  x <- liftIO $ randomRIO (a,b)
  y <- liftIO $ randomRIO (a,b)
  return $ Range (min x y) (max x y)
