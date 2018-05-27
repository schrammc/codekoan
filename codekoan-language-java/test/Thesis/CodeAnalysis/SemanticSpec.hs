{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Semantic.IdentifierSplitterSpec where

import Test.Hspec

import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Semantic.Internal
import Thesis.CodeAnalysis.Language.Java
import Thesis.Search.AlignmentMatch
import Thesis.Data.Range


spec :: SpecWith ()
spec = do
  describe "The semantic module with langauge java" $ do
    idRetrievalTest

idRetrievalTest =
  it "should correctly retrieve identifier words" $ do
    words <- liftIO $ runStdoutLoggingT $ getIds java (javaTokens, javaText) [alignmentMatch] Query
    words `shouldBe` ["main", "string", "args", "object", "pw", "system", "out", "println", "pw", "pw", "to", "char", "array", "system","out","println","pw"]

  where
    alignmentMatch =
      AlignmentMatch { resultTextRange = Range 0 (Text.length . langText $ javaText)
                     , resultMatchedTokens = []
                     , resultQueryRange = Range 0 numberOfTokens
                     , resultFragmentRange = Range 0 0
                     , resultLevenScore = 0
                     , resultMetaData = 0
                     }
    Just javaTokens = tokenize java javaText
    numberOfTokens = length javaTokens
    javaText :: LanguageText Java
    javaText = LanguageText . Text.pack $
               "public static void main(String[] args) {\n\
               \    Object pw = \"Password\";\n\
               \    System.out.println(\"String: \" + pw);\n\
               \\n\
               \    pw = \"Password\".toCharArray();\n\
               \    System.out.println(\"Array: \" + pw);\n\
               \}"
