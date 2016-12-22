{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data IdentifierType = TypeOrConstructor
                    | TypeVariableOrId
                      deriving (Eq, Ord, Show, Generic, Hashable)
                               
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
             | HsOpFmap
             | HsOpAp
             | HsOpBind
             | HsOpThen
             | HsOpCons
             | HsOpInfixConstructor
             | HsUnderscore
             deriving (Eq, Ord, Show, Generic, Hashable)

isHsIdentifier (HsIdentifier _) = True
isHsIdentifier _ = False
