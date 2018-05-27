{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data IdentifierType = TypeOrConstructor
                    | TypeVariableOrId
                      deriving (Eq, Ord, Show, Generic, Hashable, NFData)
                               
data HsToken = HsIdentifier IdentifierType
             | HsNumber
             | HsBackslash
             | HsContextArrow
             | HsAssign
             | HsEq
             | HsNotEq
             | HsLeftarrow
             | HsRightarrow
             | HsDoubleColon
             | HsLParen
             | HsRParen
             | HsLBrace
             | HsRBrace
             | HsLBrack
             | HsRBrack
             | HsPipe
             | HsTimes
             | HsDiv
             | HsAdd
             | HsMinus
             | HsAt
             | HsLet
             | HsWhere
             | HsIn
             | HsDo
             | HsPragma
             | HsIf
             | HsThen
             | HsElse
             | HsOperator
             | HsClass
             | HsData
             | HsDefault
             | HsModule
             | HsImport
             | HsInfix
             | HsInfixL
             | HsInfixR
             | HsType
             | HsNewtype
             | HsInstance
             | HsCase
             | HsOf
             | HsOpCompose
             | HsComma
             | HsOpFmap
             | HsOpAp
             | HsOpBind
             | HsOpThen
             | HsOpCons
             | HsOpInfixConstructor
             | HsUnderscore
             | HsCharLiteral
             | HsQualified
             | HsStringLiteral
             deriving (Eq, Ord, Show, Generic, Hashable, NFData)

isHsIdentifier (HsIdentifier _) = True
isHsIdentifier _ = False
