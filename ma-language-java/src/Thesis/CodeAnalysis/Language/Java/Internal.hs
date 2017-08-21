-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides internals for the java implementation, that are should
-- not be visible to other modules, except for testing or if you know exactly
-- what you are doing.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf#-}
module Thesis.CodeAnalysis.Language.Java.Internal ( Java
                                                  , java
                                                  , javaBlockData
                                                  ) where

import qualified Data.Vector as V
import Data.Text (Text)
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.Java.Internal.BlockAnalysis
import           Thesis.CodeAnalysis.Language.Java.Internal.Parser
import           Thesis.CodeAnalysis.Language.Java.Internal.Tokens
import           Thesis.CodeAnalysis.Language.Java.Internal.Type
import           Thesis.CodeAnalysis.Mutation
import           Thesis.Data.Range

import Debug.Trace

java :: Language Token Java 
java = Language { removeComments = LanguageText
                , tokenize = tokenizeJ
                , isTokenIdentifier = (== TokenIdentifier)
                , languageFileExtension = ".java"
                , languageName = "java"
                , languageGenBlockData = javaBlockData
                }

instance MutableLanguage Token Java where
  mutableBaseLanguage = java
  hasRelevantIndents = \_ -> False
  isRelevantIndent = \_ _ -> Nothing
  statementRanges = \_ -> javaStatementRanges

removeLoopHeads :: TokenVector Token Java -> TokenVector Token Java
removeLoopHeads v = V.fromList $ go 0 False 0
  where
    go i drop parens
      | i >= V.length v = []
      | otherwise = 
        let current = V.unsafeIndex v i
        in if | token current == TokenLoopWord && not drop -> 
                go (i+2) True 0
              | drop && (token current) == TokenRBrace && parens > 1 ->
                go (i+1) True (parens - 1)
              | drop && (token current) == TokenRBrace -> go (i+1) False 0
              | drop && (token current) == TokenLBrace -> go (i+1) False (parens+1)
              | drop -> go (i+1) True parens
              | otherwise -> current:(go (i+1) False parens)
              

javaStatementRanges :: LanguageText Java -> [Range Text]
javaStatementRanges txt =
  case tokenVMaybe of
    Nothing -> []
    Just tv' | V.length tv' <= 0 -> []
             | otherwise ->
      let tv = removeLoopHeads tv'
          lastToken = V.unsafeLast tv
          lastPos = rangeEnd $ coveredRange lastToken
      in reverse $ go tv (V.length tv - 1) 0 lastPos False
  where
    tokenVMaybe = tokenize java txt
    go tv i braces end open
      | i <= 0 =
        if open
        then let tWithRange = V.unsafeIndex tv 0
             in (Range (rangeStart $ coveredRange tWithRange) end):[]
        else []
      | otherwise =
        let currentTokenWithRange = V.unsafeIndex tv i
            nextTokenWithRange = V.unsafeIndex tv (i+1)
        in if (token $ currentTokenWithRange) `elem` [TokenSemicolon, TokenLBrace, TokenRBrace]
           then
            let Range _ s = coveredRange currentTokenWithRange
                Range nextStart _ = coveredRange nextTokenWithRange
                open' = token currentTokenWithRange == TokenSemicolon
            in if open
               then (Range nextStart end):(go tv (i-1) 0 s open')
               else go tv (i-1) 0 s open'
           else go tv (i-1) braces end open

javaString :: LanguageText Java
javaString = LanguageText "public class Test {\n\
                          \    public static void main(String[] args) {\n\
                          \     loop();\n\
                          \     System.out.println(\"Done\");\n\
                          \    }\n\
                          \\n\
                          \    public static void loop() {\n\
                          \     for (int i = 0; i < 5; i++) {\n\
                          \         for (int j = 0; j < 5; j++) {\n\
                          \             if (i * j > 6) {\n\
                          \                 System.out.println(\"Breaking\");\n\
                          \                 return;\n\
                          \             }\n\
                          \             System.out.println(i + \" \" + j);\n\
                          \         }\n\
                          \     }\n\
                          \    }\n\
                          \}"
