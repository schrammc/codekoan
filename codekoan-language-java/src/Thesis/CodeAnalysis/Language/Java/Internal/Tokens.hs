-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- A token type for the java language.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
module Thesis.CodeAnalysis.Language.Java.Internal.Tokens where

import Control.DeepSeq
import Data.Binary (Binary)
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)

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
           | TokenImport
           | TokenKeyword
           | TokenKeywordCondition
           | TokenKeywordClassStructure
           | TokenBreak
           | TokenLoopWord
           | TokenIdentifier
           | TokenPrimitive
           | TokenNumber
           | TokenStringLiteral
           | TokenCharacterLiteral
           | TokenModifier
           | TokenAnnotation
           | TokenLabel
           | TokenReturn
           | TokenBooleanValue
           deriving (Show,Eq, Ord, Generic, NFData, Enum)

instance Hashable Token where
  hashWithSalt n tok = hashWithSalt n (fromEnum tok)

instance Binary Token
