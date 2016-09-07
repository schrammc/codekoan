-- |
-- Author: Christof Schramm 2016
-- License: All rights reserved
--
-- A type for an exception that is thrown when something goes wrong during the
-- initialization of the search service
module Thesis.SearchService.InitializationException where

import Control.Monad.Catch

-- | A type for an exception that is thrown when something goes wrong during
-- service initialization
data InitializationException = InitializationException
                             deriving (Show)

instance Exception InitializationException
