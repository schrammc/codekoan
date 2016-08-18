-- | This module provides a common data type for language specific features of a
-- processing pipeline
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language ( Language(..)
                                    , LanguageText(..)
                                    , processAndTokenize
                                    , TokenWithRange(..)
                                    , TokenVector) where

import qualified Data.Vector as V

import Data.Text
import Data.Hashable (Hashable)

import Thesis.Data.Range

type TokenVector t l = V.Vector (TokenWithRange t l)

-- | Token combined with the range in a piece of language text, that it covers.
data TokenWithRange t l = TokenWithRange { coveredRange :: Range (LanguageText l)
                                         , token :: t
                                         }
                        deriving (Show, Eq)

-- | A datatype for a language polymorphic over two types
-- * @t@ a type for tokens
-- * @l@ a specialized language type
--
-- A specific instance would look somewhat like
-- > Language Token Java
data Language t l where
  Language :: (Ord t, Show t, Hashable t) =>
              { removeComments :: Text -> LanguageText l
              , normalize :: LanguageText l -> LanguageText l
              , tokenize :: LanguageText l -> Maybe (TokenVector t l)
              } -> Language t l

processAndTokenize :: Language t l
                   -> LanguageText l
                   -> Maybe (TokenVector t l)
processAndTokenize Language{..}= tokenize . normalize

-- | A type for the text representation fo program code in a langauge.
--
-- This type uses a phantom type @l@ to indicate that it belongs to a certain
-- language. Doing this prevents us from inadvertently mixing up e.g. bash and
-- java code at any point in the program.
newtype LanguageText l = LanguageText {langText :: Text}
