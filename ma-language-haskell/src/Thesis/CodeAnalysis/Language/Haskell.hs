-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation of the 'Langauge' datatype
-- for the haskell programming language. This implementation includes
-- a type 'Haskell', which is a type without content to be used as a
-- phantom type, a haskell tokenizer, and other things necessary to
-- analyze haskell code.
--
-- This module is intended to parse standard Haskell2010 Code and
-- accepts a limited set of benign syntax extensions. The tokenizer
-- will generally consume most code files, however obscure language
-- extensions may lead to incorrect results.
module Thesis.CodeAnalysis.Language.Haskell where

import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken

data Haskell

haskell :: Language HsToken Haskell
haskell = undefined
