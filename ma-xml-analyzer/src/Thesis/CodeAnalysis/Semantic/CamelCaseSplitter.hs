{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Semantic.CamelCaseSplitter ( camelCaseWords
                                                      , camelCaseWordsText
                                                      ) where

import Data.Text (Text)
import           Data.Char
import qualified Data.Text as T

-- | A text-wrapper around 'camelCaseWords'
camelCaseWordsText :: Text -> [Text]
camelCaseWordsText = (fmap T.pack) . camelCaseWords . T.unpack

-- | Spilt an identifier written in camel case into separate lowercase word
-- somewhat smartly. This only uses letters in an identifier as identified by
-- 'isLetter'
--
-- Some examples
-- @
-- camelCaseWords "runTest" = ["run", "test"]
-- camelCaseWords "connectHTTPTest" == ["connect", "http", "test"]
-- camelCaseWords "test" = ["test"]
-- camelCaseWords "doSome123Thing" = ["do", "some", "thing"]
-- @
camelCaseWords :: String -> [String]
camelCaseWords [] = []
camelCaseWords (_:[]) = []
camelCaseWords  (a:b:xs) =
  if | isLower a ->
       let (w, rest) = span isLower (a:b:xs)
       in (toLower <$> w):(camelCaseWords rest)
     | isUpper a && isLower b ->
       let (w, rest) = span isLower (b:xs)
       in (toLower <$> a:w):(camelCaseWords rest)
     | isUpper a && isUpper b ->
       let (w, rest) = span isUpper (a:b:xs)
           (x:w') = reverse w
       in case rest of
         (y:_) | not (isLetter y) -> (toLower <$> w):(camelCaseWords $ x:rest)
         _ -> (toLower <$> reverse w'):(camelCaseWords $ x:rest)
     | otherwise -> camelCaseWords $ b:xs
                   
