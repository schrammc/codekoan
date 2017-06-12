-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides internal helper functions for language processing

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thesis.CodeAnalysis.Language.Internal where

import           Control.DeepSeq
import           Data.List
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Thesis.Data.Range

import qualified Data.Vector as V

-- | Build a usable token vector from a list of tokens, along with how many
-- characters the token covers.
buildTokenVector :: [(Int, Maybe t)] -> TokenVector t l
buildTokenVector res =
  let (_, tokens) = mapAccumL f 0 res
  in V.fromList $ mapMaybe (\(r, t) -> TokenWithRange r <$> t) tokens
  where
    f n (k,t) = let x = n+k in x `seq` (x, (Range n x, t))

type TokenVector t l = V.Vector (TokenWithRange t l)


-- | Token combined with the range in a piece of language text, that it covers.
data TokenWithRange t l =
  TokenWithRange { coveredRange :: {-# UNPACK #-}!(Range (LanguageText l))
                 , token :: {-# UNPACK #-} !t
                 }
  deriving (Show, Eq)

-- | A type for the text representation fo program code in a langauge.
--
-- This type uses a phantom type @l@ to indicate that it belongs to a certain
-- language. Doing this prevents us from inadvertently mixing up e.g. bash and
-- java code at any point in the program.
newtype LanguageText l = LanguageText {langText :: Text}
                       deriving (NFData, Show, Eq)
