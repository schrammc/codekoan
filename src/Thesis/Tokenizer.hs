{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Thesis.Tokenizer where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad((>=>))

import Data.Char
import Data.Maybe (catMaybes)

import qualified Data.Vector as V

import Data.Text
import Data.Attoparsec.Text as AP hiding (empty)

data Language t l where
  Language :: (Ord t, Show t) => { removeComments :: Text -> LanguageText l
                                 , normalize :: LanguageText l -> LanguageText l
                                 , tokenize :: LanguageText l -> Maybe [t]
                                 } -> Language t l

processAndTokenize :: Language t l -> Text -> Maybe (V.Vector t)
processAndTokenize Language{..} = buildTokens >=> toVector
  where
    buildTokens = tokenize . normalize . removeComments
    toVector = return . V.fromList

data Java

java :: Language Token Java
java = Language { removeComments = LanguageText
                , normalize = id
                , tokenize = tokenizeJ
                }

tokenizeJ t = case parseOnly (many' ((AP.takeWhile isHorizontalSpace) *> tokenOrComment)) (langText t) of
  Right r -> Just $ catMaybes r
  _ -> Nothing
  where
    tokenOrComment = ((const Nothing) <$> (javaComment <|> endOfLine)) <|>(Just <$> token)

newtype LanguageText l = LanguageText {langText :: Text}

data Token = TokenLT
           | TokenGT
           | TokenEQ
           | TokenAssign
           | TokenNot
           | TokenBinAnd
           | TokenAnd
           | TokenBinOr
           | TokenOr
           | TokenAdd
           | TokenSub
           | TokenMult
           | TokenDiv
           | TokenMod
           | TokenLBrace
           | TokenRBrace
           | TokenLBrack
           | TokenRBrack
           | TokenLParen
           | TokenRParen
           | TokenDot
           | TokenColon
           | TokenQuestion
           | TokenSemicolon
           | TokenKeyword
           | TokenIdentifier
           | TokenNumber
           | TokenStringLiteral
           deriving (Show,Eq, Ord)

token :: Parser Token
token = "<" *> pure TokenLT
        <|> ">" *> pure TokenGT
        <|> "==" *> pure TokenEQ
        <|> "=" *> pure TokenAssign
        <|> "!" *> pure TokenNot
        <|> "&" *> pure TokenBinAnd
        <|> "&&" *> pure TokenAnd
        <|> "|" *> pure TokenBinOr
        <|> "||" *> pure TokenOr
        <|> "+" *> pure TokenAdd
        <|> "*" *> pure TokenMult
        <|> "-" *> pure TokenSub
        <|> "/" *> pure TokenDiv
        <|> "%" *> pure TokenMod
        <|> "(" *> pure TokenLParen
        <|> ")" *> pure TokenRParen
        <|> "[" *> pure TokenLBrack
        <|> "]" *> pure TokenRBrack
        <|> "{" *> pure TokenLBrace
        <|> "}" *> pure TokenRBrace
        <|> "." *> pure TokenDot
        <|> ":" *> pure TokenColon
        <|> "?" *> pure TokenQuestion
        <|> ";" *> pure TokenSemicolon
        <|> keyword
        <|> identifier
        <|> tokenNumber
        <|> stringLiteral
        

keyword :: Parser Token
keyword = ("if"
           <|> "else"
           <|> "import"
           <|> "switch"
           <|> "case"
           <|> "class"
           <|> "interface"
           <|> "null") *> pure TokenKeyword

identifier :: Parser Token
identifier = do
  satisfy nonDigit
  AP.takeWhile (\c -> or $ ($ c) <$> [isAlphaNum, nonDigit])
  return TokenIdentifier
  where
    nonDigit c = isAlpha c || c == '_' || c == '$'

tokenNumber :: Parser Token
tokenNumber = do
  scientific
  char 'd' <|> char 'f' <|> pure undefined
  return TokenNumber

javaComment :: Parser ()
javaComment = lineComment <|> blockComment
  where
    lineComment = do
      string "//"
      let content = endOfLine <|>
                    endOfInput <|>
                    (do
                        xs <- AP.takeWhile (not . isEndOfLine)
                        if xs == "" then return () else content )
      content
      return ()
    blockComment = do
      string "/*"
      let content = (string "*/" *> return ()) <|>
                    (char '*' *> content) <|>
                    (do
                        xs <- AP.takeWhile (/= '*')
                        if xs == "" then return () else content)
      content
      return ()

stringLiteral :: Parser Token
stringLiteral = do
  char '\"'
  literalContent
  return TokenStringLiteral
  where
    literalContent = do
      AP.takeWhile (\c -> not $ c `elem` ['\\','\"'])
      (char '\"' *> pure ())
        <|> (string "\\\"" *> literalContent)
        <|> (anyChar *> literalContent)

      
