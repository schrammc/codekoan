-- |
-- Description: A typeclass for code fragment descriptors
-- Maintainer: Christof Schramm
-- License: All rights reserved
-- Copyright: (c) Christof Schramm, 2016, 2017
-- Stability: Experimental
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# Language FlexibleContexts #-}
module Thesis.Search.FragmentData where

import Thesis.Data.Stackoverflow.Answer
import Control.DeepSeq
import Data.Hashable

class ( Eq (FragmentId a), Hashable (FragmentId a)
      , Hashable a, NFData a, Ord a) => FragmentData a where
  type FragmentId a
  fragDataTokenLength :: a -> Int
  printFragData :: a -> String
  getFragmentId :: a -> FragmentId a

instance FragmentData (Int, Int) where
  type FragmentId (Int, Int) = Int
  printFragData (ann,_) = show ann
  fragDataTokenLength (_, l) = l
  getFragmentId (i, _) = i


instance FragmentData AnswerFragmentMetaData where
  type FragmentId AnswerFragmentMetaData = AnswerFragmentId
  printFragData (AnswerFragmentMetaData AnswerFragmentId{..} _) =
    (show $ answerIdInt fragmentAnswerId) ++ " (" ++ (show $ fragmentId) ++ ")"
  fragDataTokenLength AnswerFragmentMetaData{..} = fragmentMetaSize
  getFragmentId = fragmentMetaId
