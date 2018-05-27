module Main where

import Data.Binary

import Thesis.Search.CompressedTrie ()
import Thesis.Data.Stackoverflow.Dictionary
import Thesis.Search.Index

import System.Environment

main :: IO ()
main = do
  [dictPath, xmlFilePath, outputPath] <- getArgs
  undefined
  {-
  dict <- readDictionary dictPath
  let nGramSize = 10
  index <- buildIndexForJava dict xmlFilePath nGramSize

  let trie = indexTrie index

  putStrLn "Done with readin the trie. Proceeding with encoding..."

  encodeFile outputPath trie
  -}
