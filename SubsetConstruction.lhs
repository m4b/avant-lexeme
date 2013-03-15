\subsection{Subset Construction}

In this module we provide our solution for converting a given non-deterministic finite state automaton to an equivalent deterministic finite state automaton.

\begin{code}

{-# LANGUAGE FlexibleInstances #-}
module SubsetConstruction(subsetConstruction) where

import Data.Maybe
import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace


type LabelMap = M.Map (S.Set Int) Int

subsetConstruction :: (Ord a, Show a) => NFA' a -> DFA' a
subsetConstruction nfa = 
    DFA' (alphabet nfa) dfamap' accept start' where
        start' = labels M.! startStateSet
        accept = findAccepting nfa labelmap
        (_, labelmap, dfamap') = 
            subsetConstruction' nfa next
                                labels dfamap outSets
        startStateSet = closure nfa (start nfa)  
        (labels, next) = 
            labelSets 0 M.empty 
            (S.fromList (startStateSet:outSets))
        edges = edgeMap labels edgeSet
        dfamap = 
            M.singleton (labels M.! startStateSet) edges
        outSets = 
            map (closure' . flip move' startStateSet)
                alphabet'
        edgeSet = zip alphabet' outSets
        alphabet' = S.toList . alphabet $ nfa
        closure' = closure nfa
        move' = move nfa

findAccepting :: (Ord a, Show a) => 
                 NFA' a -> LabelMap -> S.Set Int
findAccepting nfa labels = S.fromList sets where
  sets = M.elems (M.filterWithKey isAccepting labels)
  isAccepting label _ = 
      S.empty /= (S.intersection accept label)
  accept = accepting nfa

subsetConstruction' :: (Ord a, Show a) =>
                       NFA' a -> Int -> LabelMap -> 
                       DFAMap a -> [S.Set Int] ->
                       (Int, LabelMap, DFAMap a)
subsetConstruction' _ next labels dfamap [] = 
    (next, labels, dfamap)
subsetConstruction' nfa next labels dfamap (s:ss) = 
    case (s == S.empty) of
  True -> subsetConstruction' nfa next labels dfamap ss
  False -> if done then continue else recursion where
    done = M.lookup (labels M.! s) dfamap /= Nothing
    continue = subsetConstruction' nfa next labels dfamap ss
    recursion = 
        subsetConstruction' nfa next'' labels'' dfamap'' ss
    (next'', labels'', dfamap'') = 
        subsetConstruction' nfa next' labels' dfamap' outSets
    (labels', next') =
        labelSets next labels (S.fromList outSets)
    dfamap' = M.insert (labels M.! s) edges dfamap
    edges = edgeMap labels' edgeSet
    edgeSet = zip alphabet' outSets
    outSets = map (closure' . flip move' s) alphabet'
    alphabet' = S.toList . alphabet $ nfa
    move' = move nfa
    closure' = closure nfa

labelSets :: Int -> M.Map (S.Set Int) Int ->
             S.Set (S.Set Int) ->
            (M.Map (S.Set Int) Int, Int)
labelSets next labels sets = 
    labelSets' next labels (S.toList sets)
labelSets' :: Int -> LabelMap -> 
              [S.Set Int] -> (LabelMap, Int)
labelSets' next labels [] = (labels, next)
labelSets' next labels (s:ss) = 
  case (s == S.empty) of
       True -> labelSets' next labels ss
       False -> 
           if (M.member s labels) then 
               (labelSets' next labels ss)
           else 
               (labelSets' (next+1) 
                (M.insert s next labels) ss)

edgeMap :: (Ord a, Show a) =>
           M.Map (S.Set Int) Int ->
               [(a, (S.Set Int))] -> M.Map a Int 
edgeMap = edges' M.empty where
  edges' :: (Ord a, Show a) =>
            M.Map a Int -> M.Map (S.Set Int) Int ->
            [(a, (S.Set Int))] -> M.Map a Int 
  edges' acc _ [] = acc
  edges' acc labels ((alpha, set):ss) = 
      case (set == S.empty) of
    True -> edges' acc labels ss
    False -> edges' acc' labels ss where
      acc' = M.insert alpha (labels M.! set) acc

class Constructable c where
  closure :: (Show a, Ord a) => NFA' a -> c -> S.Set Int
  move :: (Show a, Ord a) => NFA' a -> a -> c -> S.Set Int

instance Constructable Int where
  closure nfa state = 
      fst . closure' nfa (S.singleton state) M.empty $ state where
    closure' nfa acc memoize state = 
      case (M.lookup (acc, state) memoize) of
        Nothing -> 
           if done then (acc', memoize') else (acc'', memoize'') where
             done = edges == Nothing || eps == S.singleton state
             edges = M.lookup state . trans $ nfa
             eps = S.union (S.singleton state)
                 (S.map snd . S.filter isEpsilon . 
                  fromJust $ edges)
             eps' = S.difference eps acc
             isEpsilon (label, _) = label == epsilon
             acc' = S.union acc (S.singleton state)
             acc'' = S.union acc' . S.unions $ sets
             memoize' = M.insert (acc, state) acc' memoize
             (memoize''', sets) = memoMap memoize (closure' nfa acc') . S.toList $ eps'
             memoize'' = M.insert (acc, state) acc'' memoize'''
        Just set -> (set, memoize)
        
  move nfa sym state =
    if (edges == Nothing) then S.empty else eps where
    edges = M.lookup state. trans $ nfa
    eps = S.map snd . S.filter isSym . fromJust $ edges
    isSym (label, _) = 
        label /= Nothing && sym == fromJust label      

--func x = S.map (fst . (closure' memoize acc' nfa))

memoMap :: M.Map k v -> (M.Map k v -> Int -> (v, M.Map k v)) -> [Int] -> ((M.Map k v), [v])
memoMap = memoMap' [] where
  memoMap' :: [v] -> M.Map k v -> (M.Map k v -> Int -> (v, M.Map k v)) -> [Int] -> ((M.Map k v), [v])
  memoMap' acc m _ [] = (m, acc)
  memoMap' acc m f (x:xs) = memoMap' (a:acc) m' f xs where 
    (a, m') = f m x

instance Constructable (S.Set Int) where
  closure nfa states = concatMap' (closure nfa) states
  move nfa sym states = concatMap' (move nfa sym) states

concatMap' :: (Ord a, Ord b) => 
              (a -> S.Set b) -> S.Set a -> S.Set b
concatMap' f = S.unions . S.toList . S.map f

simpleNFA :: NFA' Char
simpleNFA = NFA' alpha trans accept st where
  alpha   = S.empty
  accept  = S.fromList [0]
  st      = 0
  trans   = M.fromList [(0, trans0), (1, trans1)] where
    trans0 = S.fromList [(Nothing, 1)]
    trans1 = S.fromList [(Nothing, 0)]

testNFA :: NFA' Char
testNFA = NFA' alpha trans accept st where
  alpha   = S.empty
  accept  = S.fromList [0, 1]
  st      = 0
  trans   = M.fromList [(0, trans0), (1, trans1), (2, trans2)] where
    trans0 = S.fromList [(Nothing, 1), (Nothing, 2)]
    trans1 = S.fromList [(Nothing, 2), (Nothing, 0)]
    trans2 = S.fromList [(Nothing, 0), (Nothing, 1)]

testNFA' :: NFA' Char
testNFA' = NFA' alpha trans accept st where
  alpha   = S.fromList "a"
  accept  = S.fromList [0, 1]
  st      = 0
  trans   = M.fromList [(0, trans0), (1, trans1), (2, trans2)] where
    trans0 = S.fromList [(Nothing, 1), (Just 'a', 1), (Just 'a', 2)]
    trans1 = S.fromList [(Just 'a', 2), (Just 'a', 0)]
    trans2 = S.fromList [(Just 'a', 0), (Just 'a', 1)]

\end{code}