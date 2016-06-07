{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.StackoverflowBodyParser where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Text.HTML.DOM
import           Text.XML.Cursor

-- | Reads everything in code tags in the given text parsed as HTML
readCodeFromHTMLPost :: Text -> [Text]
readCodeFromHTMLPost t = readData <$> (fromDocument htmlDoc $// findCode)
  where
    readData = Text.concat . content
    htmlDoc = parseSTChunks [t]
    findCode = element "code" >=> child
