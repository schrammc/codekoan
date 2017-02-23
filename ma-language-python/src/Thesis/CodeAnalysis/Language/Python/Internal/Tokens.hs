-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- A token type for the python language.
--
-- THIS INTERNAL MODULE CAN BE SUBJECT TO BREAKING CHANGE AT ANY TIME. DO NOT
-- USE IT IN STABLE CODE.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
module Thesis.CodeAnalysis.Language.Python.Internal.Tokens where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data PyToken = PyTokenIndent
             | PyTokenUnindent
             | PyTokenLT
             | PyTokenGT
             | PyTokenEQ
             | PyTokenNEQ
             | PyTokenAssign
             | PyTokenNot
             | PyTokenIn
             | PyTokenBinAnd
             | PyTokenAnd
             | PyTokenBinOr
             | PyTokenBinXOR
             | PyTokenOr
             | PyTokenBinComplement
             | PyTokenAdd
             | PyTokenSub
             | PyTokenPow
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
             | PyTokenIf
             | PyTokenElse
             | PyTokenElif
             | PyTokenTry
             | PyTokenExcept
             | PyTokenFinally
             | PyTokenRaise
             | PyTokenAs
             | PyTokenWith
             | PyTokenDef
             | PyTokenPass
             | PyTokenYield
             | PyTokenSemicolon
             | PyTokenKeyword
             | PyTokenClass
             | PyTokenImport
             | PyTokenBreak
             | PyTokenLoopWord
             | PyTokenIdentifier
             | PyTokenNumber
             | PyTokenStringLiteral
             | PyTokenCharacterLiteral
             | PyTokenModifier
             | PyTokenDecorator
             | PyTokenIs
             | PyTokenReturn
             deriving (Eq, Ord, Show, Generic, Hashable, NFData)
