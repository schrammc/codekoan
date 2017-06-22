-- |
-- Maintainer : Christof Schramm
-- Description: An efficient implementation of Levenshtein automata
-- Stability  : Experimental
-- Copyright: (c) Christof Schramm, 2016, 2017
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

import           Thesis.Search.CompressedTrie

import           Data.Foldable
import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq, ViewL(..), (><) )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Vector as V

--------------------------------------------------------------------------------
--
-- Implementation of efficient Levenstein Automata

-- | A levenstein automaton for any indexable data structure. Take care, that
-- the levenSize is correct, otherwise the automaton will produce bad results.
data LevensteinAutomaton a =
  LevensteinAutomaton { levenSize :: {-#UNPACK#-} !Int
                        -- ^ Size of the input string
                      , levenN :: {-# UNPACK #-}! Int
                        -- ^ Max number of tolerated errors
                      , levenIndex :: !(Int -> a)
                        -- ^ Indexing function for the Cgiven string
                      }

vectorToLevensteinAutomaton :: Int -> V.Vector a -> LevensteinAutomaton a
vectorToLevensteinAutomaton n v =
  LevensteinAutomaton { levenSize = V.length v
                      , levenN = n
                      , levenIndex = V.unsafeIndex v
                      }

-- | A map from indices to values
newtype LevenState = LevenState {stateList :: [(Int,Int)]}
                   deriving (Show,Eq)

-- | The start state of the given levenstein automaton
startL :: Eq a => LevensteinAutomaton a -> LevenState
startL (LevensteinAutomaton{..}) = LevenState $ values
  where
    n = min levenSize levenN
    values = zip [0..n] [0..n]

-- | Step a Levenstein State once. Takes O(levenN) steps to perform once as
-- there can only ever be 2*levenN + 1 fields in a state that have a value
-- less than or equal to levenN
stepL :: (Eq a) => LevensteinAutomaton a -> LevenState -> a -> LevenState
--stepL LevensteinAutomaton{..} (LevenState []) _ = LevenState []
stepL LevensteinAutomaton{..} st@(LevenState{..}) x
  | null stateList = st
  | otherwise = LevenState $ goThroughLine initialElement stateList
  where
    initialElement = case stateList of
      (i,v):_ | v < levenN -> Just (i, v+1)
      _                    -> Nothing

    goThroughLine :: Maybe (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    goThroughLine _ [] = []
    goThroughLine l ((i,v): rest) = 
      let cost = if levenIndex i == x then 0 else 1
          val = let fromLeft = case l of
                      Just (_,v') -> min (v'+1)
                      _           -> id
                    fromTop = case rest of
                      (i',v'):_ | i' == i+1 -> min (v'+1)
                      _                     -> id
                    in fromTop $! fromLeft (v + cost)
      in if i < levenSize && val <= levenN
         then ((i+1),val):(goThroughLine (Just ((i+1),val)) rest)
         else goThroughLine l rest

-- | Predicate to determine if the given automaton state is accepting
acceptL :: LevensteinAutomaton a -> LevenState -> Bool
acceptL LevensteinAutomaton{..} LevenState{..} =
  case stateList of
    [] -> False
    _:_ -> let (i, _) = last stateList in i == levenSize


-- | Returns if the given levenstein state is an accepting state of the given
-- automaton
canAcceptL :: LevensteinAutomaton a -> LevenState -> Bool
canAcceptL aut LevenState{..} = if stateList == []
                                then False
                                else foldl1 min (snd <$> stateList) <= (levenN aut)

-- | If the state is an accepting state, return the levenstein distance of the input
acceptScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptScoreL LevensteinAutomaton{..} LevenState{..} =
  case stateList of
    _:_ -> let (i,v) = last stateList
           in if i == levenSize
              then Just v
              else Nothing
    [] -> Nothing

-- | If the state is a non - rejecting state return the minimum levenstein distance
acceptAllScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptAllScoreL LevensteinAutomaton{..} LevenState{..} =
  let !x = snd $! minimumBy (\(_,a) (_,b) -> compare a b) stateList
  in if null stateList || x > levenN
     then Nothing
     else Just x

-- | Find all words accepted by the given levenstein automaton in the trie. The
-- number for each word is the word's levenstein distance to the given word.
lookupL :: (Hashable a, Eq a, Eq v) => LevensteinAutomaton a
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
lookupAllL :: (Hashable a, Eq a, Eq v) => LevensteinAutomaton a
           -> CompressedTrie a v
           -> Seq (Seq a, v,Int)
lookupAllL aut t | t == empty = Seq.empty
                 | otherwise = lookupWithL' acceptAllScoreL aut t (startL aut)

-- | Helper function for 'lookupL'
lookupWithL' :: (Hashable a, Eq a, Eq v)
                => (LevensteinAutomaton a -> LevenState -> Maybe Int)
             ->  LevensteinAutomaton a
             -> CompressedTrie a v
             -> LevenState
             -> Seq (Seq a, v, Int)
lookupWithL' acceptScore aut (CTrieLeaf v) st =
  maybe Seq.empty (\score -> Seq.singleton (Seq.empty,v,score)) (acceptScore aut st)
lookupWithL' acceptScore aut (CTrieNode mp v) st = cur >< do
  (_,(xs, _, t)) <- Seq.fromList $ M.toList mp
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

lookupAllSuff :: (Hashable a, Eq a, Eq v) => LevensteinAutomaton a
              -> CompressedTrie a (S.Set v)
              -> Int -- ^ Minimum match length
              -> Seq (Int, Seq (S.Set v, Int), Int)
lookupAllSuff !aut trie !minMatchLength
  | trie == empty = Seq.empty
  | otherwise = do
      lookupSuff acceptAllScoreL
                 aut
                 trie
                 (startL aut)
                 0
                 minMatchLength

-- NOTE: THIS APPEARS TO BE IDENTICAL WITH lookupWithL' EXCEPT FOR THE DEPTH
-- TRACKING. ONE OF THE TWO SHOULD THEREFORE BE SCRAPPED!
lookupSuff :: (Hashable a, Eq a, Eq v)
              => (LevensteinAutomaton a -> LevenState -> Maybe Int)
           -> LevensteinAutomaton a
           -> CompressedTrie a (S.Set v)
           -> LevenState
           -> Int
           -- ^ Depth
           -> Int
           -- ^ Minimal result depth
           -> Seq (Int, Seq (S.Set v, Int) , Int)
lookupSuff acceptScore !aut nd !st !d !minDepth =
  case nd of
    CTrieNode mp curVal
      | levenN aut == 0 -> case stateList st of
        [] -> Seq.empty
        ((i,_):[]) | i < levenSize aut ->
                     let !k = levenIndex aut i
                     in case M.lookup k mp of
                       Nothing -> cur nd
                       Just br ->
                         cur (CTrieNode (M.delete k mp) curVal) >< oneBranch br
                  | i == levenSize aut -> cur nd
                  | otherwise ->
                     error "Levenshtein.lookupSuff impossible case (i > levenSize)"
        _  -> error "Levenshtein.lookupSuff impossible case (length stateList > 1)"
      | otherwise ->
        let mp' = M.mapMaybe f mp
            f br = let rs = oneBranch br
                   in if Seq.null rs
                      then Nothing
                      else Just $! rs
            results = mconcat $ M.elems mp'
            -- Nodes for which we have no results
            otherNodes = snd <$> filter (\(k,_) -> k `M.member` mp') (M.toList mp)
            curValues = mconcat $ (\(_, _, nd') -> cur nd') <$> otherNodes
        in curValues >< results
    CTrieLeaf v    ->
      maybe Seq.empty
            (\score -> Seq.singleton (d, Seq.singleton (v, 0),score))
            (acceptScore aut st)
  where
    oneBranch (label, labelLength, node) =
      let !walkResult = if levenN aut == 0
                        then walkThroughZero aut st label labelLength
                        else walkThrough acceptScore aut st label labelLength
      in case walkResult of
        LevenDone st' ->
          lookupSuff acceptScore
                     aut
                     node
                     st'
                     (d + labelLength)
                     minDepth
        LevenPartial (nMatched, levenDist) ->
          let !k = V.length label - nMatched
          in if | nMatched == 0 -> Seq.empty
                | nMatched >  0 && d + nMatched >= minDepth ->
                  let !valuesFiltered = do
                        (s, d) <- trieLeavesDist node
                        let !d' = d+k
                        if d' > minDepth then return (s,d') else Seq.empty
                      !resultDepth = d + nMatched
                  in return ( resultDepth
                            , valuesFiltered
                            , levenDist)
                | nMatched > 0 -> Seq.empty
                | otherwise -> error $ "Levenshtein.lookupSuff: Matched " <>
                                       "a negative amount of characters!"
    cur node =
          let !score = case acceptScore aut st of
                        Just s  -> Seq.singleton s
                        Nothing -> Seq.empty
              !hits = if d > minDepth
                      then do
                        _ <- score -- Do nothing if we fail to calculate the score
                        trieLeavesDist node
                      else Seq.empty
          in if Seq.null hits
             then Seq.empty
             else case score of
               (Seq.viewl -> EmptyL) -> Seq.empty
               (Seq.viewl -> (s :< _)) -> Seq.singleton (d, hits ,s)

data LevenResult = LevenDone !LevenState
                 | LevenPartial {-# UNPACK #-} !(Int, Int)
                   -- The tuple contains:
                   --   * The number of matched tokens
                   --
                   --   * The levenshtein score of the
                   --      automaton

walkThrough :: (Hashable a, Eq a) =>
               (LevensteinAutomaton a -> LevenState -> Maybe Int)
            -> LevensteinAutomaton a
            -> LevenState
            -> V.Vector a
            -> Int
            -> LevenResult
walkThrough acceptScore aut state v !vectorLength =
  walkThrough' (acceptScore aut state) 0 state
  where
    walkThrough' lastScore !i !st =
      if i == vectorLength
      then LevenDone st
      else
        let st' = stepL aut st (V.unsafeIndex v i)
            scoreMaybe = acceptScore aut st'
        in case scoreMaybe of
          Just _ -> walkThrough' scoreMaybe (i+1) st'
          Nothing    -> case lastScore of
            Just x -> x `seq` LevenPartial (i,x)
            Nothing -> error "Levenshtein.walkthrough: unexpected failure!"

-- An optimized version of 'walkThrough' for levenshtein distance 0.
--
-- ** This function has no defined behaviour for levenshtein distances /= 0 and
-- will likely call 'error'. It may also return incorrect results in special
-- cases. **
walkThroughZero :: (Hashable a, Eq a) =>
                   LevensteinAutomaton a
                -> LevenState
                -> V.Vector a
                -> Int
                -> LevenResult
walkThroughZero aut (LevenState ((pos,val):[])) v !vectorLength =
  walkThroughZero' 0
  where
    walkThroughZero' !i =
      if i == vectorLength
      then let !pos' = pos+vectorLength
           in LevenDone (LevenState [(pos', val)])
      else let !j = pos + i
           in if j < levenSize aut && V.unsafeIndex v i == levenIndex aut j
              then walkThroughZero' (i+1)
              else LevenPartial (i, 0)
walkThroughZero _ _ _ _ =
  error "Levenshtein.walkThroughZero: unexpected case (dist /= 0)"
