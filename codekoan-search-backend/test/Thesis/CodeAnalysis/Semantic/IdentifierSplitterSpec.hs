{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Semantic.IdentifierSplitterSpec where

import Test.Hspec

import Thesis.CodeAnalysis.Semantic.IdentifierSplitter

spec :: SpecWith ()
spec = do
  describe "Thesis.CodeAnalysis.Semantic.IdentifierSplitter" $ do
    example1
    example2
    example3
    example4

example1 = it "example (camel case)" $
  splitIdText "runTest" `shouldBe` ["run", "test"]

example2 = it "example (snake case)" $
  splitIdText "run_test_stuff" `shouldBe` ["run", "test", "stuff"]

example3 = it "example (camel case caps)" $
  splitIdText "testHTTPConnection" `shouldBe` ["test", "http", "connection"]

example4 = it "example (camel / snake case mix)" $
  splitIdText "testNew_HTTPConnection" `shouldBe`
  ["test", "new" , "http", "connection"]
