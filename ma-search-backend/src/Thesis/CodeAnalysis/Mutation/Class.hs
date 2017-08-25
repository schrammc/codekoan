{-# LANGUAGE MultiParamTypeClasses #-}
module Thesis.CodeAnalysis.Mutation.Class where

import Data.Text (Text)
import Thesis.CodeAnalysis.Language
import Thesis.Data.Range
import Thesis.CodeAnalysis.Mutation.Base
import Control.Monad.Random

data GenericIndentation = GenericIndent
                        | GenericUndindent

class MutableLanguage t l where
  mutableBaseLanguage :: Language t l
  statementRanges :: Language t l -> LanguageText l -> [Range Text]
  hasRelevantIndents :: Language t l -> Bool
  isRelevantIndent :: Language t l -> t -> Maybe GenericIndentation
-- | Pick a random statement in the given source code and remove it. If the
-- source code contains no identifiable statement the original source is
-- returned.
removeRandomStatement :: (MutableLanguage t l, MonadRandom m)
                         => Language t l -> LanguageText l -> m (LanguageText l)
removeRandomStatement lang txt =
  let rgs = statementRanges lang txt
  in case rgs of
    [] -> return txt
    _  -> do
      rg <- head <$> randomShuffle rgs
      return . LanguageText $ deleteAt rg (langText txt)

-- | Picks two random statements in the given source code and swaps them. If the
-- given source code does not contain at least two identifiable statements
swapRandomStatements :: (MutableLanguage t l, MonadRandom m)
                        => Language t l -> LanguageText l -> m (LanguageText l)
swapRandomStatements lang txt = do
  let rgs = statementRanges lang txt
  shuffledRanges <- randomShuffle rgs
  case shuffledRanges of
    [] -> return txt
    _:[] -> return txt
    x:y:_ ->
      let a = min x y
          b = max x y
          [textA, textB] = textInRanges (langText txt) [a,b]
      in return . LanguageText
         $ replaceAt textB a
         $ replaceAt textA b (langText txt)

