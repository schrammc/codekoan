-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module contians exceptions that can happen (even though they absolutely
-- shouldn't) in processing of a search query
module Thesis.SearchException where

import Control.Monad.Catch

-- | An exception that can occur in semantic processing
newtype SemanticException = SemanticException String
                          deriving (Show)

instance Exception SemanticException
