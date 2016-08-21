-- | This module provides a common data type for language specific features of a
-- processing pipeline
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language ( Language(..)
                                    , LanguageText(..)
                                    , processAndTokenize
                                    , TokenWithRange(..)
                                    , TokenVector
                                    , identifiers) where

import qualified Data.Vector as V
import qualified Data.Text as T

import Data.Text (Text)
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
              { languageFileExtension :: String
              , removeComments :: Text -> LanguageText l
              , normalize :: LanguageText l -> LanguageText l
                -- ^ Normalize a piece of language text. This function should be
                -- idempotent. I.e. it should hold that normalize txt == normalize $
                -- normalize txt
              , tokenize :: LanguageText l -> Maybe (TokenVector t l)
              , isTokenIdentifier :: t -> Bool
              } -> Language t l

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
      zip tksList (texts 0 normalizedText (coveredRange <$> tksList))
    tksList = V.toList tks
    -- Helper function to get all ranges in a text in one pass because text has
    -- /O(n)/ random access
    texts _ _ [] = []
    texts pos t ((Range start stop):rs)  =
      let t' = T.drop (start-pos) t
          (tkText, restText) = T.splitAt (stop-start) t'
      in tkText:(texts stop restText rs)

-- | A type for the text representation fo program code in a langauge.
--
-- This type uses a phantom type @l@ to indicate that it belongs to a certain
-- language. Doing this prevents us from inadvertently mixing up e.g. bash and
-- java code at any point in the program.
newtype LanguageText l = LanguageText {langText :: Text}
