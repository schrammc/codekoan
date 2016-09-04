 {-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Thesis.Search.Levenstein where

import Data.List

import Thesis.Search.CompressedTrie

import qualified Data.Map.Strict as M
import Data.Foldable
import qualified Data.Set as S

import Data.Monoid ((<>))
import qualified Data.Vector as V

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
stepL LevensteinAutomaton{..} (LevenState []) _ = LevenState []
stepL LevensteinAutomaton{..} LevenState{..} x =
  LevenState $ reverse $ foldl f lst statesWithSuccessors
  where
    -- Successors of positions in the state. The last position has no successor
    successors = (Just <$> (tail stateList))++[Nothing]

    --  Pairs of state positions with their successor positions
    statesWithSuccessors = (zip stateList successors)

    -- If we can extend the state of the first position downwards, we do
    lst = case head stateList of
      (i,v) | v < levenN -> [(i, v+1)]
      _                  -> []

    f l ((i,v), suc) = let cost = if levenIndex i == x then 0 else 1
                           fromLeft = case l of
                             (_,v'):_ -> ((v'+1):)
                             _        -> id
                           fromTop = case suc of
                             Just (i',v') | i' == i+1 -> ((v'+1):)
                             _                        -> id
                           val = foldl1 min (fromTop $ fromLeft [v + cost])
                        in if i < levenSize && val <= levenN
                           then ((i+1),val):l
                           else l

-- | Predicate to determine if the given automaton state is accepting
acceptL :: LevensteinAutomaton a -> LevenState -> Bool
acceptL LevensteinAutomaton{..} LevenState{..} =
  case reverse stateList of
    [] -> False
    (i,_):_ -> i == levenSize

-- | Returns if the given levenstein state is an accepting state of the given
-- automaton
canAcceptL :: LevensteinAutomaton a -> LevenState -> Bool
canAcceptL aut LevenState{..} = if stateList == []
                                then False
                                else foldl1 min (snd <$> stateList) <= (levenN aut)

-- | If the state is an accepting state, return the levenstein distance of the input
acceptScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptScoreL LevensteinAutomaton{..} LevenState{..} =
  case reverse stateList of
    (i,v):_ | i == levenSize -> Just v
    _ -> Nothing

-- | If the state is a non - rejecting state return the minimum levenstein distance
acceptAllScoreL :: LevensteinAutomaton a -> LevenState -> Maybe Int
acceptAllScoreL LevensteinAutomaton{..} LevenState{..} =
  let x = minimum $ snd <$> stateList
  in if null stateList || x > levenN
     then Nothing
     else Just x

-- | Find all words accepted by the given levenstein automaton in the trie. The
-- number for each word is the word's levenstein distance to the given word.
lookupL :: (Ord a, Eq v) => LevensteinAutomaton a
        -> CompressedTrie a v
        -> [([a], v,Int)]
lookupL aut t | t == empty = []
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
           -> [([a], v,Int)]
lookupAllL aut t | t == empty = []
                 | otherwise = lookupWithL' acceptAllScoreL aut t (startL aut)

-- | Helper function for 'lookupL'
lookupWithL' :: (Ord a, Eq v)
                => (LevensteinAutomaton a -> LevenState -> Maybe Int)
             ->  LevensteinAutomaton a
             -> CompressedTrie a v
             -> LevenState
             -> [([a], v, Int)]
lookupWithL' acceptScore aut (CTrieLeaf v) st =
  maybe [] (\score -> [([],v,score)]) (acceptScore aut st)
lookupWithL' acceptScore aut (CTrieNode mp v) st = cur ++ do
  (_,(xs, t)) <- M.toList mp
  newState <- toList $ foldlM f st xs
  extend (V.toList xs) <$> lookupWithL' acceptScore aut t newState
  where
   f st c | canAcceptL aut st = Just $! stepL aut st c
          | otherwise = Nothing
   extend cs (cs', v', s) = (cs <> cs', v', s)
   cur = toList ( ([],,) <$> v <*> acceptScore aut st)

lookupAllSuff :: (Ord a, Ord v) => LevensteinAutomaton a
           -> CompressedTrie a (S.Set v)
           -> [([a], [(S.Set v, Int)], Int)]
lookupAllSuff aut t | t == empty = []
                    | otherwise =
  lookupSuff acceptAllScoreL aut t (startL aut) 0

-- NOTE: THIS APPEARS TO BE IDENTICAL WITH lookupWithL' EXCEPT FOR THE DEPTH
-- TRACKING. ONE OF THE TWO SHOULD THEREFORE BE SCRAPPED!
lookupSuff :: (Ord a, Ord v)
              => (LevensteinAutomaton a -> LevenState -> Maybe Int)
           -> LevensteinAutomaton a
           -> CompressedTrie a (S.Set v)
           -> LevenState
           -> Int -- ^ Depth
           -> [([a], [(S.Set v, Int)] , Int)]
lookupSuff acceptScore aut (CTrieLeaf v) st _ =
  maybe [] (\score -> [([],[(v, 0)],score)]) (acceptScore aut st)
lookupSuff acceptScore aut nd@(CTrieNode mp _) st d = cur ++ do
  (_,(xs, t)) <- M.toList mp
  newState <- toList $ foldlM f st xs
  extend (V.toList xs) <$> lookupSuff acceptScore aut t newState (d + length xs)
  where
   f st c | canAcceptL aut st = Just $! stepL aut st c
          | otherwise = Nothing
   extend cs (cs', v', s) = (cs <> cs', v', s)
   cur = let score = toList $ acceptScore aut st
             hits = if d > 10
                    then do
                      _ <- score -- Do nothing if we fail to calculate the score
                      trieLeavesDist nd
                    else []
         in case hits of
           [] -> []
           hs@(_:_) -> length hs `seq` [([], hs ,)] <*> score

eliminateRedundantHits :: (Ord a, Ord v) =>
                          [([a], S.Set v, Int)]
                       -> [([a], S.Set v, Int)]
eliminateRedundantHits xs =
  concat $ processGroup <$> distGroups
  where
    sortHits = reverse . (sortOn (\(tks,_,_) -> tks))
    -- Hits grouped by levensteinDistance
    distGroups = sortHits <$> groupBy (\(_,_,d) -> \(_,_,d') -> d == d') xs

    groupPrefixes [] = []
    groupPrefixes xs@((tks, _, _):_) =
       let (group, rest) = span (\(tks',_,_) -> isPrefixOf tks' tks) xs
       in group : (groupPrefixes rest)


    processGroup group = concat $ (processPGroup S.empty) <$> (groupPrefixes group)

    -- This function operates on answers of the same score where every answer's
    -- tokens are a prefix of it's succesor's tokens
    --
    -- The purpose is to let every single hit for an id only occur for the
    -- longest substring and not for every prefix
    processPGroup _      [] = []
    processPGroup aIdSet ((tks, aIds, s):rest) =
     let diffSet = aIds `S.difference` aIdSet
         unionSet = aIds `S.union` aIdSet
     in case diffSet of
            x | S.null x  ->  processPGroup aIdSet rest
              | otherwise ->  (tks, diffSet, s):(processPGroup unionSet rest)
