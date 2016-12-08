module Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken where

data HsToken = HsIdentifier
             | HsNumber
             | HsAssign
             | HsLeftarrow
             | HsRightarrow
             | HsDoubleColon
             | HsLParen
             | HsRParen
             | HsLBrace
             | HsRBrace
             | HsLBrack
             | HsRBrack
