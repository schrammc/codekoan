-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation of the 'Langauge' datatype for the
-- python language. This implementation includes a type 'Python', which is a
-- type without content to be used as a phantom type, a python tokenizer, and
-- other things necessary to analyze pythoncode.
--
-- This is designed to work with both python 2 and python 3. If a case should
-- arise where a result is either incorrect for python 2 or python 3, always
-- expect the python 2 result to be incorrect. 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings#-}
module Thesis.CodeAnalysis.Language.Python ( python
                                           , Python
                                           , buildIndexForPython
                                           ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Attoparsec.Text as AP
import           Data.Char
import           Data.Conduit (Source)
import           Data.Maybe (fromJust)
import qualified Data.Text as Text

import           Thesis.Search.Index
import           Thesis.Data.Stackoverflow.Answer

import           Thesis.Search
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.CommonTokenParsers
import           Thesis.CodeAnalysis.Language.Internal
import           Thesis.CodeAnalysis.Language.Python.Internal.Tokens
import           Thesis.CodeAnalysis.Language.Python.Internal.BlockAnalysis

data Python

python :: Language PyToken Python
python = Language{ languageFileExtension = ".py"
                 , languageName = "python"
                 , tokenize = tokenizePy
                 , isTokenIdentifier = (== PyTokenIdentifier)
                 , removeComments = LanguageText
                 , languageGenBlockData = pythonBlockData
                 }

buildIndexForPython :: ( MonadIO m
                       , MonadLogger m) => 
                    Source m Answer
                    -- ^ A source of answers with python code. To get this use
                    -- one of the Thesis.Data.Stackoverflow.Dump.* modules
                    -> Int -- ^ NGram size
                    -> m (SearchIndex PyToken Python AnswerFragmentMetaData)
buildIndexForPython = buildIndexFromAnswers python


-- | State that the will use internally.
data ParserState = ParserState{ blockWidth :: Maybe Int
                              , indentationLevel :: Int
                              }

-- | Tokenize a piece of python code. It is critical, that this code is
-- normalized beforehand. Normalization eliminates all tab characters in the
-- code and replaces them with tabs. The normalized code is then /much/ easier
-- to parse. Not that this parser will accept correct python 3 code but also a
-- lot of code that violates some of the indentation rules or things that python
-- might enforce.
tokenizePy :: LanguageText Python -> Maybe (TokenVector PyToken Python)
tokenizePy LanguageText{..} = buildTokenVector <$> parsedResult
  where
    parsedResult = case AP.parseOnly parseCode langText of
      Right x -> Just x
      Left _  -> Nothing
    
    parseCode :: Parser [(Int, Maybe PyToken)]
    parseCode = evalStateT parsePy (ParserState Nothing 0) 

    -- | Recursive parser that parses an entire file while strining along some
    -- parser state. That state contains information on the depth of the current
    -- code block. This parser assumes that it always starts at a fresh line.
    parsePy :: StateT ParserState Parser [(Int, Maybe PyToken)]
    parsePy = do
      -- Generate indentation tokens if necessary
      spaces <- getSpaces

      -- Parse the line's contents
      tks <- lift $ AP.many1 lenParser

      --  Parse a number of linebreaks after the content of the line
      lbs <- (lift $ ((:[]) <$> eols)) <|> (return [])

      
      rest <- parsePy <|> (lift $ endOfInput *> pure [])

      let tokens = case tks of
            [] -> (filter ((> 0) . fst) spaces) ++ lbs
            _  -> spaces ++ tks ++ lbs
      return $ tokens ++ rest

    -- | Parses tokens, comments and horizontal whitespace along with
    -- information on the length of the consumed string.
    lenParser :: Parser (Int, Maybe PyToken)
    lenParser = do
      (txt, token) <- AP.match tokenOrComment
      return $ (Text.length txt, token)

    tokenOrComment = ((const Nothing) <$> commentOrSpace) <|> (Just <$> tokenP)
    commentOrSpace = pythonLineComment <|>
                     (AP.takeWhile1 isHorizontalSpace *> pure ())
    eols = do
      (txt, _) <- AP.match (AP.takeWhile1 isEndOfLine)
      return $ (Text.length txt, Nothing)

    -- | This helper function is for parsing spaces at the beginning of
    -- lines. It generates the 0-width indentatoin tokens.
    getSpaces:: StateT ParserState Parser [(Int, Maybe PyToken)]
    getSpaces = do
      spacesWithTabs <- lift $ AP.takeWhile isHorizontalSpace
      let spaces = Text.replace "\t" "    " spacesWithTabs

      st@ParserState{..} <- get
      let n = Text.length spaces
          same = n == indentationLevel
          close = n < indentationLevel
          open  = n > indentationLevel

      when (blockWidth == Nothing && n > 0) $ put st{blockWidth = Just n}

      st@ParserState{..} <- get
      let valid = case blockWidth of
                    Nothing -> True
                    Just x  -> gcd n x > 1
          delta = indentationLevel - n

      put st{indentationLevel = n}

      if valid
      then if | n == 0 && same -> return []
              | same -> return [(n, Nothing)]
              | open -> return [(0, Just PyTokenIndent), (n, Nothing)]
              | close ->
                  let lst = replicate (delta `div` (fromJust blockWidth))
                                      (0,Just PyTokenUnindent)
                  in return $ lst ++ [(n,Nothing)]
              | n > 0 && open -> undefined
      else fail "Indentation fault"

tokenP :: Parser PyToken
tokenP =     "<"  *> pure PyTokenLT
         <|> ">"  *> pure PyTokenGT
         <|> "==" *> pure PyTokenEQ
         <|> "="  *> pure PyTokenAssign
         <|> "!=" *> pure PyTokenNEQ
         <|> ":"  *> pure PyTokenColon
         <|> ","  *> pure PyTokenComma
         <|> "."  *> pure PyTokenDot
         <|> "+"  *> pure PyTokenAdd
         <|> "-"  *> pure PyTokenSub
         <|> "**" *> pure PyTokenPow
         <|> "*"  *> pure PyTokenMult
         <|> "/"  *> pure PyTokenDiv
         <|> "%"  *> pure PyTokenMod
         <|> "("  *> pure PyTokenLParen
         <|> ")"  *> pure PyTokenRParen
         <|> "["  *> pure PyTokenLBrack
         <|> "]"  *> pure PyTokenRBrack
         <|> "{"  *> pure PyTokenLBrace
         <|> "}"  *> pure PyTokenRBrace
         <|> "not" *> pure PyTokenNot
         <|> "and" *> pure PyTokenAnd
         <|> "in"  *> pure PyTokenIn
         <|> "or"  *> pure PyTokenOr
         <|> "class" *> pure PyTokenClass
         <|> "import" *> pure PyTokenImport
         <|> "if" *> pure PyTokenIf
         <|> "else" *> pure PyTokenElse
         <|> "elif" *> pure PyTokenElif
         <|> "try" *> pure PyTokenTry
         <|> "except" *> pure PyTokenExcept
         <|> "finally" *> pure PyTokenFinally
         <|> "raise" *> pure PyTokenRaise
         <|> "as"    *> pure PyTokenAs
         <|> "with"  *> pure PyTokenWith
         <|> "def"   *> pure PyTokenDef
         <|> "pass"  *> pure PyTokenPass
         <|> "yield" *> pure PyTokenYield
         <|> loopWord
         <|> pyStringLiteral
         <|> pyNumber
         <|> identifier
         <|> ("@" *> identifier *> pure PyTokenDecorator)

pyStringLiteral :: Parser PyToken
pyStringLiteral = ((characterLiteral *> pure ()) <|>
                   (stringLiteral *> pure ())
                  ) *> pure PyTokenStringLiteral

identifier :: Parser PyToken
identifier = do
  satisfy nonDigit
  AP.takeWhile (\c -> or $ ($ c) <$> [isAlphaNum, nonDigit])
  return PyTokenIdentifier
  where
    nonDigit c = isAlpha c || c == '_'

loopWord :: Parser PyToken
loopWord = ("for" <|> "while") *> pure PyTokenLoopWord

pyNumber :: Parser PyToken
pyNumber =  scientific *> pure PyTokenNumber
