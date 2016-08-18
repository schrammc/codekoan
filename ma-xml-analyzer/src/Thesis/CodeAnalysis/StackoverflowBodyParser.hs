-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module is for reading information from stackoverflow post bodies. This
-- functionality is in a separate module from question and answer because it is
-- the same for all stackoverflow post types.
{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.StackoverflowBodyParser where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Text.HTML.DOM
import           Text.XML.Cursor

-- | Parses the given text as HTML and returns the content of all code tags in
-- or der of their occurence
readCodeFromHTMLPost :: Text -> [Text]
readCodeFromHTMLPost t = readData <$> (fromDocument htmlDoc $// findCode)
  where
    readData = Text.concat . content
    htmlDoc = parseSTChunks [removeBreaks t]
    findCode = element "code" >=> descendant
    -- TODO: Find out what happens if there is HTML in the code.
    -- TODO: How is that then escaped ?
    -- TODO: Maybe it's inconsistent for different times...
    
    -- | Ridiculously sometimes <br> is used in code tags as a linebreak.  In order
    -- to not mess up our result we replace it with an actual linebreak.
    removeBreaks = Text.replace "<br>" "\n"
