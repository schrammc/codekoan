{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings#-}
module Thesis.CodeAnalysis.Mutation.Class where

import           Control.Monad.Random
import           Data.Text (Text)
import qualified Data.Text as Text
import           Thesis.CodeAnalysis.Language
import           Thesis.CodeAnalysis.Mutation.Base
import           Thesis.Data.Range

data GenericIndentation = GenericIndent
                        | GenericUndindent

class MutableLanguage t l where
  mutableBaseLanguage :: Language t l
  statementRanges :: Language t l -> LanguageText l -> [Range Text]
  hasRelevantIndents :: Language t l -> Bool
  isRelevantIndent :: Language t l -> t -> Maybe GenericIndentation

buildStatementCorpus :: (MutableLanguage t l)
                        => Language t l
                     -> [LanguageText l]
                     -> [LanguageText l]
buildStatementCorpus lang txts = fmap LanguageText $ do
  txt <- txts
  let ranges = (statementRanges lang txt)
  textInRanges (langText txt) ranges

insertRandomStatementFromCorpus :: (MutableLanguage t l, MonadRandom m)
                                   => Language t l
                                   -> [LanguageText l]
                                   -> LanguageText l
                                   -> m (LanguageText l)
insertRandomStatementFromCorpus lang stmts txt = do
  stmtMaybe <- pickRandom stmts
  case stmtMaybe of
    Nothing -> return txt
    Just stmt -> insertAtRandomStatement lang stmt txt

insertAtRandomStatement :: (MutableLanguage t l, MonadRandom m)
                           => Language t l
                           -> LanguageText l
                           -> LanguageText l
                           -> m (LanguageText l)
insertAtRandomStatement lang piece txt = do
  let stRanges = statementRanges lang txt
  stMaybe <- pickRandom stRanges
  case stMaybe of
    Nothing -> -- Couldn't find a statement, we just append the code.
      return $ LanguageText $ Text.append (langText txt) (langText piece)
    Just (Range _ end) ->
      let endPosition = TextPosition $ fromIntegral end
          insertIndent = indentAt (langText txt) endPosition
          insertText = Text.append "\n" (indentTo insertIndent (langText piece))
      in return $ LanguageText $ insertAt insertText endPosition (langText txt)

-- | Pick a random statement in the given source code and remove it. If the
-- source code contains no identifiable statement the original source is
-- returned.
removeRandomStatement :: (MutableLanguage t l, MonadRandom m)
                         => Language t l -> LanguageText l -> m (LanguageText l)
removeRandomStatement lang txt = do
  let rgs = statementRanges lang txt
  pickedMaybe <- randomPick rgs
  case pickedMaybe of
    Nothing -> return txt
    Just rg -> do
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

