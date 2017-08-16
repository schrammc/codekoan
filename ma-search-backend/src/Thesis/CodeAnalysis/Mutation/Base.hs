{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Mutation.Base where

import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.String
import           Data.Text as Text
import qualified Data.Vector.Unboxed as VUnboxed
import           Thesis.Data.Range
import Text.Read
newtype TextPosition = TextPosition {posToInt :: Int}
                     deriving (Show, Eq, Ord)

newtype VString = VString {vstringToVector :: VUnboxed.Vector Char}
                deriving (Eq, Ord)

instance Show VString where
  show = VUnboxed.toList . vstringToVector

instance Read VString where
  readPrec = do
    x <- readPrec
    return $ fromString x

instance IsString VString where
  fromString = VString . VUnboxed.fromList

textToVString :: Text -> VString
textToVString  = fromString . Text.unpack

randomPosition :: MonadIO m => VString -> m TextPosition
randomPosition = undefined

shiftToLineStart :: VString -> TextPosition -> TextPosition
shiftToLineStart t pos = TextPosition . go $ posToInt pos
  where
    go k | k <= 0 = 0
         | otherwise = let c = VUnboxed.unsafeIndex (coerce t) (k - 1)
                       in if c == '\n' then k else go (k - 1)

shiftToNextLineStart :: VString -> TextPosition -> TextPosition
shiftToNextLineStart str pos = TextPosition . go $ posToInt pos + 1
  where
    t = vstringToVector str
    go k | k >= (VUnboxed.length t - 1) = (VUnboxed.length t - 1)
         | otherwise = let c = VUnboxed.unsafeIndex t (k - 1)
                       in if c == '\n' then k else go (k + 1)

indentLevelAt :: VString -> TextPosition -> Int
indentLevelAt t pos' = (posToInt end) - (posToInt pos)
  where
    pos = shiftToLineStart t pos'
    end = moveRightWhile (`elem` [' ', '\t']) t pos

moveLeftWhile  :: (Char -> Bool) -> VString -> TextPosition -> TextPosition
moveLeftWhile pred str pos = TextPosition . go $ posToInt pos
  where
    go k | k <= 0 = 0
         | otherwise = if pred $ VUnboxed.unsafeIndex (coerce str) k
                       then go (k - 1)
                       else k

moveRightWhile :: (Char -> Bool) -> VString -> TextPosition -> TextPosition
moveRightWhile pred str pos = TextPosition . go $ posToInt pos
  where
    t = vstringToVector str
    go k | k >= VUnboxed.length t - 1 = VUnboxed.length t - 1
         | otherwise = if pred $ VUnboxed.unsafeIndex t k
                       then go (k + 1)
                       else k

getCurrentLine :: VString -> TextPosition -> VString
getCurrentLine str pos =
  let startPos = shiftToLineStart     str pos
      endPos   = shiftToNextLineStart str pos
  in VString $ VUnboxed.slice (posToInt startPos)
                              (posToInt endPos - posToInt startPos)
                              (coerce str)

vstringInRange :: Range VString -> VString -> VString
vstringInRange (Range a b) str =
  coerce $ VUnboxed.unsafeSlice a (b - 1) (coerce str)

indentBy :: Int -> VString -> VString
indentBy k str = undefined

minIndentLevel :: VString -> Int
minIndentLevel str = undefined
