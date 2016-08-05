{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.CodeAnalysis.Language (Language(..), LanguageText(..), processAndTokenize, buildTokenVector) where

import qualified Data.Vector as V

import Data.Conduit
import Data.Conduit.Attoparsec (PositionRange)
import qualified Data.Conduit.List as CL

import Data.Text
import Data.Hashable (Hashable)

data Language t l where
  Language :: (Ord t, Show t, Hashable t) =>
              { removeComments :: Text -> LanguageText l
              , normalize :: LanguageText l -> LanguageText l
              , tokenize ::Conduit (LanguageText l) Maybe (PositionRange, t)
              } -> Language t l


buildTokenVector :: Language t l -> LanguageText l -> Maybe (V.Vector t)
buildTokenVector l@Language{..} t =
  V.fromList . (fmap snd) <$> processAndTokenize l t

processAndTokenize :: Language t l -> LanguageText l-> Maybe [(PositionRange, t)]
processAndTokenize Language{..} t =
  (yield (normalize t) $$ tokenize =$ CL.consume)

newtype LanguageText l = LanguageText {langText :: Text}
