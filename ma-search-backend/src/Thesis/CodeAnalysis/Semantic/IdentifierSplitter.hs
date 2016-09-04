-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- A module that splits identifiers first by underscores and then those split
-- results by camel case.
module Thesis.CodeAnalysis.Semantic.IdentifierSplitter where

import Data.Text (Text)
import qualified Data.Text as T
import Thesis.CodeAnalysis.Semantic.CamelCaseSplitter

import Data.List.Split (splitOn)

-- | A 'T.Text' version of 'splitId'
splitIdText :: Text -> [Text]
splitIdText = (fmap T.pack) . splitId  . T.unpack

-- | Split identifiers first by underscore and then by camel case. This function
-- assumes that the string that is passed in doesn't contain whitespace.
splitId :: String -> [String]
splitId identifier = do
  idPart <- filter (not . null) (splitOn "_" identifier)
  camelCaseWords idPart
