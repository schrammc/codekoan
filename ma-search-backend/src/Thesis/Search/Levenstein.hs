-- |
-- Maintainer : Christof Schramm
-- Description: An efficient implementation of Levenshtein automata
-- Stability  : Experimental
-- License    : All rights reserved
--
-- This module contains an implementation of levenshtein automata and an
-- implementation of inexact, greedy search in suffix tries using levenshtein
-- automata.
--
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.Search.Levenstein where

import Thesis.Search.CompressedTrie

import qualified Data.Map.Strict as M
import Data.Foldable
import qualified Data.Set as S

import Data.Monoid ((<>))
import qualified Data.Vector as V
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), (><) )
import qualified Data.Sequence as Seq
--------------------------------------------------------------------------------
--
-- Implementation of efficient Levenstein Automata

-- | A levenstein automaton for any indexable data structure. Take care, that
-- the levenSize is correct, otherwise the automaton will produce bad results.
data LevensteinAutomaton a =
  LevensteinAutomaton { levenSize :: Int -- ^ Size of the input string
                      , levenN :: Int    -- ^ Max number of tolerated errors
                      , levenIndex :: Int -> a -- ^ Indexing function for the
                                               -- given string
                      }

vectorToLevensteinAutomaton :: Int -> V.Vector a -> LevensteinAutomaton a
vectorToLevensteinAutomaton n v =
  LevensteinAutomaton { levenSize = V.length v
                      , levenN = n
                      , levenIndex = V.unsafeIndex v
                      }

-- | A map from indices to values
newtype LevenState = LevenState {stateList :: Seq (Int,Int)}
                   deriving (Show,Eq)

-- | The start state of the given levenstein automaton
startL :: Eq a => LevensteinAutomaton a -> LevenState
startL (LevensteinAutomaton{..}) = LevenState $ values
  where
    n = min levenSize levenN
    values = Seq.fromList $ zip [0..n] [0..n]

-- | Step a Levenstein State once. Takes O(levenN) steps to perform once as
-- there can only ever be 2*levenN + 1 fields in a state that have a value
-- less than or equal to levenN
stepL :: (Eq a) => LevensteinAutomaton a -> LevenState -> a -> LevenState
--stepL LevensteinAutomaton{..} (LevenState []) _ = LevenState []
stepL LevensteinAutomaton{..} st@(LevenState{..}) x
  | Seq.null stateList = st
  | otherwise = 
  LevenState $ goThroughLine initialElement stateList
  where
    initialElement = case Seq.viewl stateList of
      (i,v) :< _ | v < levenN -> Just (i, v+1)
      _                       -> Nothing

    goThroughLine :: Maybe (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
    goThroughLine l (Seq.viewl -> EmptyL) = Seq.empty
    goThroughLine l (Seq.viewl -> (i,v) :< rest) = 
      let cost = if levenIndex i == x then 0 else 1
          fromLeft = case l of
            Just (_,v') -> min (v'+1)
            _           -> id
          fromTop = case Seq.viewl rest of
           (i',v') :< _ | i' == i+1 -> min (v'+1)
           _                        -> id
          val = fromTop $ fromLeft (v + cost)
          
      in if i < levenSize && val <= levenN
         then ((i+1),val) <| (goThroughLine (Just ((i+1),val)) rest)
         else goThroughLine l rest

-- | Predicate to determine if the given automaton state is accepting
acceptL :: LevensteinAutomaton a -> LevenState -> Bool
acceptL LevensteinAutomaton{..} LevenState{..} =
  case Seq.viewr stateList of
    EmptyR -> False
    _ :> (i,_) -> i == levenSize

-- | Returns if the given levenstein state is an accepting state of the given
-- automaton
canAcceptL :: LevensteinAutomaton a -> LevenState -> Bool
canAcceptL aut LevenState{..} = if Seq.null stateList
                                then False
                                else foldl1 min (snd <$> stateList) <= (levenN aut)

-- | If the state is an accepting state, return the levenstein distance of the input
acceptScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptScoreL LevensteinAutomaton{..} LevenState{..} =
  case Seq.viewr stateList of
    _ :> (i,v) | i == levenSize -> Just v
    _ -> Nothing

-- | If the state is a non - rejecting state return the minimum levenstein distance
acceptAllScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptAllScoreL LevensteinAutomaton{..} LevenState{..} =
  let x = snd $ minimumBy (\(_,a) (_,b) -> compare a b) stateList
  in if null stateList || x > levenN
     then Nothing
     else Just x

-- | Find all words accepted by the given levenstein automaton in the trie. The
-- number for each word is the word's levenstein distance to the given word.
lookupL :: (Ord a, Eq v) => LevensteinAutomaton a
        -> CompressedTrie a v
        -> Seq (Seq a, v,Int)
lookupL aut t | t == empty = Seq.empty
              | otherwise = lookupWithL' acceptScoreL aut t (startL aut)

-- | Find all words in the given trie, that are not rejected by the levenstein
-- automaton. Not that this is drastically different from 'lookupL' in that it
-- will not only yield words of approximately 'levenSize' but also all shorter
-- words, i.e. that are a prefix of the word for which the levenstein automaton
-- is constructed if the levenstein distance to a prefix of the levenstein word
-- is no larger then 'levenN'.
--
-- In this function the levenstein automaton is transformed, so that all
-- non-rejecting states are accepting.
lookupAllL :: (Ord a, Eq v) => LevensteinAutomaton a
           -> CompressedTrie a v
           -> Seq (Seq a, v,Int)
lookupAllL aut t | t == empty = Seq.empty
                 | otherwise = lookupWithL' acceptAllScoreL aut t (startL aut)

-- | Helper function for 'lookupL'
lookupWithL' :: (Ord a, Eq v)
                => (LevensteinAutomaton a -> LevenState -> Maybe Int)
             ->  LevensteinAutomaton a
             -> CompressedTrie a v
             -> LevenState
             -> Seq (Seq a, v, Int)
lookupWithL' acceptScore aut (CTrieLeaf v) st =
  maybe Seq.empty (\score -> Seq.singleton (Seq.empty,v,score)) (acceptScore aut st)
lookupWithL' acceptScore aut (CTrieNode mp v) st = cur >< do
  (_,(xs, t)) <- Seq.fromList $ M.toList mp
  newState <- case foldlM f st xs of
                Just x  -> Seq.singleton x
                Nothing -> Seq.empty
  extend (Seq.fromList $ V.toList xs) <$> lookupWithL' acceptScore aut t newState
  where
   f st c | canAcceptL aut st = Just $! stepL aut st c
          | otherwise = Nothing
   extend cs (cs', v', s) = (cs >< cs', v', s)
   cur = case ( (Seq.empty,,) <$> v <*> acceptScore aut st) of
           Just x  -> Seq.singleton x
           Nothing -> Seq.empty

lookupAllSuff :: (Ord a, Ord v) => LevensteinAutomaton a
           -> CompressedTrie a (S.Set v)
           -> Int -- ^ Minimum match length
           -> Seq (Seq a, Seq (S.Set v, Int), Int)
lookupAllSuff aut trie minMatchLength | trie == empty = Seq.empty
                                      | otherwise = do
  (tks, values, score) <- lookupSuff acceptAllScoreL
                                     aut
                                     trie
                                     (startL aut)
                                     (0, minMatchLength)
  return $ length tks `seq` (tks, values, score)

-- NOTE: THIS APPEARS TO BE IDENTICAL WITH lookupWithL' EXCEPT FOR THE DEPTH
-- TRACKING. ONE OF THE TWO SHOULD THEREFORE BE SCRAPPED!
lookupSuff :: (Ord a, Ord v)
              => (LevensteinAutomaton a -> LevenState -> Maybe Int)
           -> LevensteinAutomaton a
           -> CompressedTrie a (S.Set v)
           -> LevenState
           -> (Int, Int) -- ^ (Depth, Minimal result depth)
           -> Seq (Seq a, Seq (S.Set v, Int) , Int)
lookupSuff acceptScore aut (CTrieLeaf v) st _ =
  maybe Seq.empty
        (\score -> Seq.singleton (Seq.empty, Seq.singleton (v, 0),score))
        (acceptScore aut st)
lookupSuff acceptScore aut nd@(CTrieNode mp _) !st (d, minDepth) =
  foldl' (><) cur (oneBranch <$> M.elems mp)
  where
    oneBranch (xs, t) = 
      case walkThrough acceptScore aut st xs of
        LevenDone st' -> {-# SCC levenDoneCase #-}
          extend (fromV xs) <$> lookupSuff acceptScore
                                              aut
                                              t
                                              st'
                                              (d + length xs, minDepth)
        LevenPartial (nMatched, levenDist) ->
          let k = (V.length xs - nMatched)
          in if | nMatched == 0 -> Seq.empty
                | nMatched >  0 && d + nMatched >= minDepth ->
                  let valuesFiltered = do
                        (s, d) <- trieLeavesDist t
                        let d' = d+k
                        if d' > minDepth
                          then return (s,d')
                          else Seq.empty
                  in Seq.singleton ( Seq.fromFunction
                                       nMatched
                                       (V.unsafeIndex (V.take nMatched xs))
                                   , valuesFiltered
                                   , levenDist)
                | nMatched > 0 -> Seq.empty
                | otherwise -> error $ "Levenshtein.lookupSuff: Matched " <>
                                       "a negative amount of characters!"
    fromV v = Seq.fromFunction (V.length v) (V.unsafeIndex v)
    extend cs (cs', v', s) = (cs >< cs', v', s)
    cur = let score = case acceptScore aut st of
                        Just s  -> Seq.singleton s
                        Nothing -> Seq.empty
              hits = if d > minDepth
                     then do
                       _ <- score -- Do nothing if we fail to calculate the score
                       trieLeavesDist nd
                     else Seq.empty
          in if Seq.null hits
             then Seq.empty
             else length hits `seq` Seq.singleton (Seq.empty, hits ,) <*> score

data LevenResult = LevenDone !LevenState
                 | LevenPartial !(Int, Int) -- The tuple contains:
                                            --   * The number of matched tokens
                                            --
                                            --   * The levenshtein score of the
                                            --      automaton

walkThrough :: (Eq a) =>
               (LevensteinAutomaton a -> LevenState -> Maybe Int)
            -> LevensteinAutomaton a
            -> LevenState
            -> V.Vector a
            -> LevenResult
walkThrough acceptScore automaton state v =
  if V.null v
  then error "Levenshtein.walkthrough: empty vector!"
  else walkThrough' 0 automaton state
  where
    vectorLength = V.length v
    walkThrough' !i !aut !st =
      if i == vectorLength
      then LevenDone st
      else
        let st' = stepL aut st (V.unsafeIndex v i)
        in case acceptScore aut st' of
          Just _ -> walkThrough' (i+1) aut st'
          Nothing    -> case acceptScore aut st of
            Just x -> LevenPartial (i,x)
            Nothing -> error "Levenshtein.walkthrough: unexpected failure!"
