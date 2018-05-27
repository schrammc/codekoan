-- | A collection of helper functions for parsing the Stackoverflow XML Dump
module Thesis.Data.Stackoverflow.XML.Helpers where

import Data.Attoparsec.Text

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.XML.Types
import           Text.XML.Stream.Parse

-- | Require an attribute and 'read' it
readAttribute :: Read a => Data.XML.Types.Name -> AttrParser a
readAttribute x = (read . Text.unpack) <$> requireAttr x

-- | Try parsing with attoparsec from the given text and fill in the default value
-- if parsing fails
parseWithDefault :: a -> Parser a -> Text -> a
parseWithDefault def p t = (either (const def) (id)) $ parseOnly p t
