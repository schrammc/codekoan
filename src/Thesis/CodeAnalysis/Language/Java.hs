{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Thesis.CodeAnalysis.Language.Java where

import           Control.Applicative ((<|>))

import           Data.Attoparsec.Text as AP

import           Data.Binary (Binary)

import           Data.Char

import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL

import           Data.Hashable (Hashable)

import           GHC.Generics (Generic)

import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Language.CommonTokenParsers

data Java

java :: Language Token Java 
java = Language { removeComments = LanguageText
                , normalize = id
                , tokenize = tokenizeJ
                }

tokenizeJ :: Conduit (LanguageText Java) Maybe (PositionRange, Token)
tokenizeJ = (CL.map langText) =$= conduitParser completeParser =$= filterNothings
  where
    completeParser = (AP.takeWhile isHorizontalSpace) *> tokenOrComment
    tokenOrComment = ((const Nothing) <$> (javaStyleComment <|> endOfLine)) <|>(Just <$> token)
    filterNothings = awaitForever $ \(pos, x) -> do
      mapM_ (yield . (pos, )) x

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
           | TokenComma
           | TokenColon
           | TokenQuestion
           | TokenSemicolon
           | TokenKeyword
           | TokenIdentifier
           | TokenPrimitive
           | TokenNumber
           | TokenStringLiteral
           | TokenCharacterLiteral
           | TokenModifier
           | TokenAnnotation
           deriving (Show,Eq, Ord, Generic)

instance Hashable Token

instance Binary Token

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
        <|> "," *> pure TokenComma
        <|> ":" *> pure TokenColon
        <|> "?" *> pure TokenQuestion
        <|> ";" *> pure TokenSemicolon
        <|> modifier
        <|> keyword
        <|> primitive
        <|> identifier
        <|> tokenNumber
        <|> stringLiteral *> pure TokenStringLiteral
        <|> characterLiteral *> pure TokenCharacterLiteral
        <|> annotation
        

keyword :: Parser Token
keyword = ("if"
           <|> "else"
           <|> "import"
           <|> "switch"
           <|> "case"
           <|> "class"
           <|> "interface"
           <|> "package"
           <|> "null"
           <|> "throw"
           <|> "catch"
           <|> "finally"
           <|> "switch"
           <|> "case"
           <|> "throws"
           <|> "super"
           <|> "this"
           <|> "new"
           <|> "break"
           <|> "continue"
           <|> "goto"
           <|> "synchronized"
           <|> "package"
           <|> "do"
           <|> "while"
           <|> "enum"
           <|> "assert"
          ) *> pure TokenKeyword

modifier :: Parser Token
modifier = ("public"
           <|> "private"
           <|> "protected"
           <|> "static"
           <|> "volatile"
           ) *> pure TokenModifier

primitive :: Parser Token
primitive = ("byte"
             <|> "char"
             <|> "short"
             <|> "int"
             <|> "long"
             <|> "float"
             <|> "double"
             <|> "boolean"
             <|> "void") *> pure TokenPrimitive

annotation :: Parser Token
annotation = char '@' *> identifier *> pure TokenAnnotation

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
