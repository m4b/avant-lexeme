\begin{code}

{-# LANGUAGE FlexibleInstances #-}
module SubsetConstruction(subsetConstruction) where

import Data.Maybe
import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S

type LabelMap = M.Map (S.Set Int) Int

subsetConstruction :: (Ord a, Show a) => NFA' a -> DFA' a
subsetConstruction nfa = DFA' (alphabet nfa) dfamap' accept start' where
  start' = labels M.! startStateSet
  accept = findAccepting nfa labelmap
  (_, labelmap, dfamap') = subsetConstruction' nfa next labels dfamap outSets
  startStateSet = closure nfa (start nfa)  
  (labels, next) = labelSets 0 M.empty (S.fromList (startStateSet:outSets))
  edges = edgeMap labels edgeSet
  dfamap = M.singleton (labels M.! startStateSet) edges
  outSets = map (closure' . flip move' startStateSet) alphabet'
  edgeSet = zip alphabet' outSets
  alphabet' = S.toList . alphabet $ nfa
  closure' = closure nfa
  move' = move nfa

findAccepting :: (Ord a, Show a) => NFA' a -> LabelMap -> S.Set Int
findAccepting nfa labels = S.fromList sets where
  sets = M.elems (M.filterWithKey isAccepting labels)
  isAccepting label _ = S.empty /= (S.intersection accept label)
  accept = accepting nfa

subsetConstruction' :: (Ord a, Show a) => NFA' a -> Int -> LabelMap -> DFAMap a -> [S.Set Int] -> (Int, LabelMap, DFAMap a)
subsetConstruction' nfa next labels dfamap [] = (next, labels, dfamap)
subsetConstruction' nfa next labels dfamap (s:ss) = case (s == S.empty) of
  True -> subsetConstruction' nfa next labels dfamap ss
  False -> if done then continue else recursion where
    done = M.lookup (labels M.! s) dfamap /= Nothing
    continue = subsetConstruction' nfa next labels dfamap ss
    recursion = subsetConstruction' nfa next'' labels'' dfamap'' ss
    (next'', labels'', dfamap'') = subsetConstruction' nfa next' labels' dfamap' outSets
    (labels', next') = labelSets next labels (S.fromList outSets)
    dfamap' = M.insert (labels M.! s) edges dfamap
    edges = edgeMap labels' edgeSet
    edgeSet = zip alphabet' outSets
    outSets = map (closure' . flip move' s) alphabet'
    alphabet' = S.toList . alphabet $ nfa
    move' = move nfa
    closure' = closure nfa

labelSets :: Int -> M.Map (S.Set Int) Int -> S.Set (S.Set Int) -> (M.Map (S.Set Int) Int, Int)
labelSets next labels sets = labelSets' next labels (S.toList sets)
labelSets' :: Int -> LabelMap -> [S.Set Int] -> (LabelMap, Int)
labelSets' next labels [] = (labels, next)
labelSets' next labels (s:ss) = 
  case (s == S.empty) of
       True -> labelSets' next labels ss
       False -> if (M.member s labels) then (labelSets' next labels ss) else (labelSets' (next+1) (M.insert s next labels) ss)

edgeMap :: (Ord a, Show a) => M.Map (S.Set Int) Int -> [(a, (S.Set Int))] -> M.Map a Int 
edgeMap = edges' M.empty where
  edges' :: (Ord a, Show a) => M.Map a Int -> M.Map (S.Set Int) Int -> [(a, (S.Set Int))] -> M.Map a Int 
  edges' acc _ [] = acc
  edges' acc labels ((alpha, set):ss) = case (set == S.empty) of
    True -> edges' acc labels ss
    False -> edges' acc' labels ss where
      acc' = M.insert alpha (labels M.! set) acc


class Constructable c where
  closure :: (Show a, Ord a) => NFA' a -> c -> S.Set Int
  move :: (Show a, Ord a) => NFA' a -> a -> c -> S.Set Int

instance Constructable Int where
  closure nfa state = closure' (S.singleton state) nfa state where
    closure' acc nfa state = if done then acc' else acc'' where
      done = edges == Nothing || eps == S.singleton state
      edges = M.lookup state . trans $ nfa
      eps = S.union (S.singleton state) (S.map snd . S.filter isEpsilon . fromJust $ edges)
      eps' = S.difference eps acc
      isEpsilon (label, _) = label == epsilon
      acc' = S.union acc (S.singleton state)
      acc'' = S.unions . S.toList . S.map (closure' acc' nfa) $ eps'
      
  move nfa sym state = if (edges == Nothing) then S.empty else eps where
    edges = M.lookup state. trans $ nfa
    eps = S.map snd . S.filter isSym . fromJust $ edges
    isSym (label, _) = label /= Nothing && sym == fromJust label      

instance Constructable (S.Set Int) where
  closure nfa states = concatMap' (closure nfa) states
  move nfa sym states = concatMap' (move nfa sym) states

concatMap' :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
concatMap' f = S.unions . S.toList . S.map f

\end{code}