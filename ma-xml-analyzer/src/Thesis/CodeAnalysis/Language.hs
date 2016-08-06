{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language (Language(..), LanguageText(..), processAndTokenize) where

import qualified Data.Vector as V

import Data.Text
import Data.Hashable (Hashable)

import Thesis.Data.Range

data Language t l where
  Language :: (Ord t, Show t, Hashable t) =>
              { removeComments :: Text -> LanguageText l
              , normalize :: LanguageText l -> LanguageText l
              , tokenize :: LanguageText l -> Maybe (V.Vector (Range, t))
              } -> Language t l

processAndTokenize :: Language t l -> LanguageText l-> Maybe (V.Vector (Range, t))
processAndTokenize Language{..}= tokenize . normalize

newtype LanguageText l = LanguageText {langText :: Text}
