-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides a common data type for language specific features of a
-- processing pipeline
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language ( -- * Basic Types
                                      Language(..)
                                    , LanguageText(..)
                                      -- * Tokens
                                    , TokenWithRange(..)
                                    , TokenVector
                                      -- * Basic functions
                                    , processAndTokenize
                                    , identifiers
                                    ) where

import Data.Text (Text)
import Data.Hashable (Hashable)
import qualified Data.Vector as V

import Thesis.Data.Range
import Thesis.CodeAnalysis.Language.Internal

-- | A datatype for a language polymorphic over two types
--
--   * @t@ a type for tokens
--
--   * @l@ a specialized language type
--
-- A specific instance would look somewhat like
-- > Language Token Java
data Language t l where
  Language :: (Ord t, Show t, Hashable t) =>
              { languageFileExtension :: String
                -- ^ Typical file extension for the given language. Examples
                -- would be ".java" for java, ".py" for python, etc.
              , removeComments :: Text -> LanguageText l
              , normalize :: LanguageText l -> LanguageText l
                -- ^ Normalize a piece of language text. This function should be
                -- idempotent. I.e. it should hold that
                -- @ normalize txt == normalize $ normalize txt @
              , tokenize :: LanguageText l -> Maybe (TokenVector t l)
              , isTokenIdentifier :: t -> Bool
                -- ^ If the token is an identifier token, the underlying
                -- identifier will be used in word similarity filtering
              } -> Language t l

-- | Normalize and then tokenize the given language text.
processAndTokenize :: Language t l
                   -> LanguageText l
                   -> Maybe (TokenVector t l)
processAndTokenize Language{..} = tokenize . normalize

-- | Get all identifiers from a code document
identifiers :: Language t l -- ^ The underlying language implementation
            -> (LanguageText l) -- ^ The code document in 'normalize' text form
            -> TokenVector t l -- ^ The tokenized document
            -> [Text]
identifiers Language{..} txt tks = do
  (TokenWithRange{..}, coveredText) <- tokensAndCovered
  if isTokenIdentifier token
    then return coveredText
    else []
  where
    normalizedText = langText $ normalize txt
    tokensAndCovered =
      zip tksList (textInRanges normalizedText
                                ((convertRange . coveredRange) <$> tksList))
    tksList = V.toList tks