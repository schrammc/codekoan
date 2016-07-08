module Thesis.StackexchangeAPI.WrapperParseException where

import Control.Exception
import Data.ByteString.Lazy as BL
import Data.Typeable (Typeable)

data WrapperParseException = WrapperParseException String BL.ByteString 
                           deriving (Typeable, Show)

instance Exception WrapperParseException where
