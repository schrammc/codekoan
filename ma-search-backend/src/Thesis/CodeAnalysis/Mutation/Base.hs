-- |
-- Copyright: Christof Schramm 2016
-- Description: Basic operations for source code mutation
-- License: All rights reserved
--
-- This module provides basic operations for source code mutation.
{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Mutation.Base where

import           Control.Monad.Random.Strict
import           Data.Char
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Thesis.CodeAnalysis.Language
import           Thesis.Data.Range

newtype TextPosition = TextPosition {posToInt :: Word}
                     deriving (Eq, Ord, Show)

indentBy :: Int -> Text -> Text
indentBy n = Text.unlines . fmap (mappend t) . Text.lines
  where
    t = Text.pack $ replicate n ' '

unindentBy :: Int -> Text -> Text
unindentBy n = Text.unlines . fmap (Text.drop n) . Text.lines

minIndent :: Text -> Int
minIndent txt =
  let lineLengths = fmap (Text.length . Text.takeWhile isSpace)
                    $ filter (not . isSpaceOnly)
                    $ Text.lines txt
  in case lineLengths of
    [] -> 0
    _  -> minimum lineLengths

isSpaceOnly :: Text -> Bool
isSpaceOnly t = all isSpace (Text.unpack t)

-- | Insert a piece of text at a given position into a text
insertAt :: Text
         -- ^ Inserted piece
         -> TextPosition
         --  ^ Insert position
         -> Text
         -- ^ The text into which we insert
         -> Text
insertAt piece pos txt =
  let (before, after) = Text.splitAt (fromIntegral $ posToInt pos) txt
  in before <> piece <> after

-- | Find out the current text indentation level at the given position. If the
-- given position is within an empty line the indent of the previous non-empty
-- line is used. If there is no previous non-empty line the result is 0
indentAt :: Text -> TextPosition -> Word
indentAt t pos = go (Text.lines t) 0 0
  where
    posInt = fromIntegral $ posToInt pos
    go []     _ ind = ind
    go (l:ls) p ind
      | isSpaceOnly l = go ls (p + Text.length l + 1) ind
      | otherwise =
        let ind' = fromIntegral $ minIndent l
        in if posInt < p + (Text.length l + 1)
           then ind'
           else go ls (p + Text.length l + 1) ind'

randomPosition :: MonadRandom m => Text -> m (TextPosition)
randomPosition t = do
  p <- getRandomR (0, Text.length t)
  return . TextPosition $ fromIntegral p

randomTextRange :: (MonadRandom m) => Text -> m (Range Text)
randomTextRange t = do
  let n = Text.length t
  a <- getRandomR (0, n)
  b <- getRandomR (0, n)
  return $ Range (min a b) (max a b)

insertAtRandomPosition :: MonadRandom m
                       => Text
                       -- ^ Inserted piece
                       -> Text
                       -- ^ whole text
                       -> m Text
insertAtRandomPosition piece txt = do
  pos <- randomPosition txt
  return $ insertAt piece pos txt

replaceAt :: Text -> Range Text -> Text -> Text
replaceAt piece range txt =
  let (before, rest) = Text.splitAt (rangeStart range) txt
      (_, after) = Text.splitAt (rangeEnd range - rangeStart range) rest
  in before <> piece <> after

-- | Delete text in a given range
deleteAt :: Range Text -> Text -> Text
deleteAt = replaceAt ""

-- | An indentifier made up of some of the given words
randomWordId :: (MonadRandom m) => V.Vector Text -> m Text
randomWordId words = do
  n <- getRandomR (1, 5)
  idWords <- replicateM n $ do
    k <- getRandomR (0, V.length words - 1)
    return $ V.unsafeIndex words k
  return $ camelConcat idWords
  where
    camelConcat xs = case Text.toLower <$> xs of
      y:ys -> mconcat $ y:(firstUpper <$> ys)
      [] -> Text.empty
    firstUpper x = case Text.unpack x of
      c:cs -> Text.pack $ (toUpper c):cs
      [] -> Text.empty
    
-- | A random identifier made up of upper and lower case letters
randomId :: (MonadRandom m) => m Text
randomId = fmap Text.pack $ do
  n <- getRandomR (1,20)
  replicateM n $ do
    c <- getRandomR ('a', 'z')
    b <- getRandom
    return $ if b then c else toUpper c

-- | Start positions of the lines in the given text
lineStartPositions :: Text -> Set TextPosition
lineStartPositions txt = S.fromList $ go (Text.lines txt) 0  
  where
    go lins n =
      let currentPos = (TextPosition $ fromIntegral n)
      in case lins of
           []   -> []
           l:ls -> currentPos:(go ls (n + Text.length l + 1))

-- | Gets the underlying text from an identifier
underlyingText :: LanguageText l -> TokenWithRange t l -> LanguageText l
underlyingText l t =
  LanguageText $ textInRange (coveredRange t) (langText l)
