module Thesis.Data.Stackoverflow.Dump where

import Thesis.Data.Stackoverflow.StackoverflowPost
import Data.Conduit
import Control.Monad.Trans.Resource

import           Text.XML.Stream.Parse

-- | Create a source of 'StackoverflowPost' s from a file
postSource :: FilePath -> Source (ResourceT IO) StackoverflowPost
postSource xmlFilePath = parseFile def xmlFilePath =$= parsePosts
