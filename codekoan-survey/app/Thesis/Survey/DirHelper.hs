module Thesis.Survey.DirHelper where

import Control.Monad
import Data.List
import System.Directory

allCodeFiles = allFiles >=> (return . filterCodeFiles)

allFiles :: FilePath -> IO [FilePath]
allFiles path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then return (path:[])
    else do
      entries <- listDirectory path
      paths <- concat <$> forM ((\p -> path ++ "/" ++ p) <$>  entries) allFiles
      return $ paths

filterCodeFiles :: [FilePath] -> [FilePath]
filterCodeFiles ps =
  filter (\p -> isSuffixOf ".java" p ||
                isSuffixOf ".py" p ||
                isSuffixOf ".hs" p
         ) ps
