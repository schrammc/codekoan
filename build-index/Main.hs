module Main where

import Data.Binary

import Thesis.Trie ()
import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Search

import System.Environment

main :: IO ()
main = do
  [dictPath, xmlFilePath, outputPath] <- getArgs
  dict <- readDictionary dictPath
  let nGramSize = 10
  index <- buildIndexForJava dict xmlFilePath nGramSize

  let trie = indexTrie index

  putStrLn "Done with readin the trie. Proceeding with encoding..."

  encodeFile outputPath trie
  
