module Main where

import Lib

import Control.Concurrent

import Control.DeepSeq

import Data.Char (chr)
import qualified Data.Vector as V

import Thesis.Search.CompressedTrie

import System.Random
import Control.Monad

main :: IO ()
main = do
  tree <- force <$> randomSuffixTree 1000

  print $ length (wordsInTrie' tree)
  print "Waiting"

  threadDelay $ 1000 * 1000

  print $ length (wordsInTrie' tree)

randomSuffixTree :: Int -> IO (CompressedTrie Char Int)
randomSuffixTree n = do
  strs <- replicateM n randomString
  let suffixTrees = (\(s,k) -> buildSuffixTrie Nothing s k) <$> zip (V.fromList <$> strs) [0..]
  return $ foldl1 mergeTries suffixTrees



randomString :: IO String
randomString = do
  n <- randomRIO (20, 500)
  replicateM n $ randomRIO (chr 0, chr 100)
