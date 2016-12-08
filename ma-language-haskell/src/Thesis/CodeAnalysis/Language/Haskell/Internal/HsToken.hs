{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

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
             deriving (Eq, Ord, Show, Generic, Hashable)
