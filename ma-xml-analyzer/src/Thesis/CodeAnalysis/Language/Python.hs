{-# LANGUAGE RecordWildCards #-}
-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation of the 'Langauge' datatype for the
-- python language. This implementation includes a type 'Python', which is a
-- type without content to be used as a phantom type, a python tokenizer, and
-- other things necessary to analyzer java code.
--
-- This is designed to work with both python 2 and python 3. If a case should
-- arise where a result is either incorrect for python 2 or python 3, always
-- expect the python 2 result to be incorrect. 

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings#-}
module Thesis.CodeAnalysis.Language.Python where

import           Control.Applicative
-- import           Control.Monad
import           Data.Char
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.CommonTokenParsers

import qualified Data.Text as Text

import           Data.Attoparsec.Text as AP

data Python

python :: Language PyToken Python
python = Language{ languageFileExtension = ".py"
                 , tokenize = tokenizePy
                 , normalize = id
                 , isTokenIdentifier = (== PyTokenIdentifier)
                 , removeComments = LanguageText
                 }

data PyToken = PyTokenIndent
             | PyTokenUnindent
             | PyTokenLT
             | PyTokenGT
             | PyTokenEQ
             | PyTokenAssign
             | PyTokenNot
             | PyTokenBinAnd
             | PyTokenAnd
             | PyTokenBinOr
             | PyTokenOr
             | PyTokenAdd
             | PyTokenSub
             | PyTokenMult
             | PyTokenDiv
             | PyTokenMod
             | PyTokenLBrace
             | PyTokenRBrace
             | PyTokenLBrack
             | PyTokenRBrack
             | PyTokenLParen
             | PyTokenRParen
             | PyTokenDot
             | PyTokenComma
             | PyTokenColon
             | PyTokenQuestion
             | PyTokenSemicolon
             | PyTokenKeyword
             | PyTokenBreak
             | PyTokenLoopWord
             | PyTokenIdentifier
             | PyTokenPrimitive
             | PyTokenNumber
             | PyTokenStringLiteral
             | PyTokenCharacterLiteral
             | PyTokenModifier
             deriving (Eq, Ord, Show, Generic, Hashable)

data SpaceType = Tabs
               | Spaces

data IndentationFault = IndentationFault

tokenizePy :: LanguageText Python -> Maybe (TokenVector PyToken Python)
tokenizePy LanguageText{..} = undefined
  where
      parsePy indentWidth spaceType = do
        spaces <- validateSpaces spaceType
        case spaces of
          Left IndentationFault -> return Nothing
          Right spaceTokens  -> do
            tks <- many' lenParser
            numberOfBreaks <- eols
            undefined
      lenParser :: Parser (Int, Maybe PyToken)
      lenParser = do
        (txt, token) <- AP.match tokenOrComment
        return $ (Text.length txt, token)

      tokenOrComment = ((const Nothing) <$> commentOrSpace) <|> (Just <$> tokenP)
      commentOrSpace = pythonLineComment <|>
                       (AP.takeWhile isHorizontalSpace *> pure ())
      eols = do
        (txt, _) <- AP.match (AP.takeWhile isEndOfLine)
        return $ Text.length txt

      validateSpaces :: SpaceType
                     -> Parser (Either IndentationFault [(Int, Maybe PyToken)])
      validateSpaces spaceType =  do
        txt <- AP.takeWhile isHorizontalSpace
        case spaceType of
          Spaces -> undefined
          Tabs   -> undefined

tokenP :: Parser PyToken
tokenP = "<"      *> pure PyTokenLT
         <|> ">"  *> pure PyTokenGT
         <|> "==" *> pure PyTokenEQ
         <|> "="  *> pure PyTokenAssign
         <|> ":"  *> pure PyTokenColon
         <|> ","  *> pure PyTokenComma
         <|> "("  *> pure PyTokenLParen
         <|> ")"  *> pure PyTokenRParen
         <|> "["  *> pure PyTokenLBrack
         <|> "]"  *> pure PyTokenRBrack
         <|> pyStringLiteral
         <|> pyCharacterLiteral
         <|> identifier
         


pyStringLiteral :: Parser PyToken
pyStringLiteral = stringLiteral *> pure PyTokenStringLiteral

-- | A standard character literal like @'a'@.
pyCharacterLiteral :: Parser PyToken
pyCharacterLiteral = characterLiteral *> pure PyTokenCharacterLiteral

identifier :: Parser PyToken
identifier = do
  satisfy nonDigit
  AP.takeWhile (\c -> or $ ($ c) <$> [isAlphaNum, nonDigit])
  return PyTokenIdentifier
  where
    nonDigit c = isAlpha c || c == '_'
