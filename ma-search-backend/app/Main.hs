module Main where

import           Control.Monad
import qualified Data.Vector as V
import           System.Random
import           Thesis.Search.CompressedTrie
import           Thesis.Search.Levenstein
import           Data.List (foldl')
import qualified Data.Set as S
import           Control.DeepSeq

main :: IO ()
main = do
  putStrLn "Building index..."
  (trie, vectors) <- randomTrie
  let onlyVectors = snd <$> vectors
  trie `deepseq` putStrLn "Index constructed. Starting benchmarks..."
  runBenchmark trie onlyVectors 500

runBenchmark :: CompressedTrie Char (S.Set Int)  -> [V.Vector Char] -> Int -> IO ()
runBenchmark trie vs n | n <= 0 = return ()
                       | otherwise = do
  k <- randomRIO (200,800)
  v <- V.take k <$> randomChoice vs
  print $ doSearch trie v
  runBenchmark trie vs (n-1)

doSearch :: CompressedTrie Char (S.Set Int) -> V.Vector Char -> Int
doSearch tr v = sum $ do
  (xs,l,n) <- resList
  xs `deepseq` n `deepseq` l `deepseq` return $ 1 + (length l)
  where
    la = LevensteinAutomaton (V.length v) 0 (V.unsafeIndex v)
    resList = lookupAllSuff la tr 7

randomTrie :: IO (CompressedTrie Char (S.Set Int), [(S.Set Int,V.Vector Char)])
randomTrie = do
  strs <- randomStrings
  
  let indexedStrings = zip (S.singleton <$> [0 ..]) strs
      tries = do
        (n, str) <- indexedStrings
        [buildSuffixTrie (Just 7) str n]
  return $ (foldl' mergeTries empty tries, indexedStrings)

randomStrings :: IO [V.Vector Char]
randomStrings = do
  strs <- replicateM 2000 randomString
  length strs `seq` return strs

randomString :: IO (V.Vector Char)
randomString = V.fromList <$> replicateM 1000 (randomRIO ('a','z'))

randomChoice :: [a] -> IO a
randomChoice lst = do
  k <- randomRIO (0, (length lst) - 1)
  return $ lst !! k
