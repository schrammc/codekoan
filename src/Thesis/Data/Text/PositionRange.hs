{-# LANGUAGE OverloadedStrings #-}
module Thesis.Data.Text.PositionRange ( PositionRange (..)
                                      , Position (..)

                                      , textFrom
                                      , textUntil
                                      , textInRange

                                      , isSubPosRangeOf
                                      ) where

import Data.Conduit.Attoparsec
import Data.Text (Text)
import qualified Data.Text as Text

-- | Text in range. (Includes both start and stop position)
textInRange :: PositionRange -> Text -> Text
textInRange (PositionRange start stop) t =
  textFrom start $ textUntil stop t

-- | Text from a certain position
textFrom :: Position -> Text -> Text
textFrom (Position l c) t =
  case textLines of
    []   -> ""
    l:ls -> let l' = Text.drop (c-1) l
            in Text.unlines $ if l' == ""
                              then ls
                              else l':ls
  
  where
    textLines = drop (l-1) $ Text.lines t

-- | Text up to a certain position (inclusive)
textUntil :: Position -> Text -> Text
textUntil (Position l c) t =
  case reverse textLines of
    []   -> ""
    l:ls -> let l' = Text.take (c) l
            in Text.unlines $ if l' == ""
                              then ls
                              else reverse $ l':ls
  
  where
    textLines = take l $ Text.lines t

-- | Return true if the second range contains the first range
isSubPosRangeOf :: PositionRange -> PositionRange -> Bool
isSubPosRangeOf (PositionRange a b) (PositionRange c d) = c <= a && d >= b
