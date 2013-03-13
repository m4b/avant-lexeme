\begin{code}

{-# LANGUAGE FlexibleInstances #-}
module SubSetConstruction(subsetConstruction) where

import Data.Maybe
import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S
import Regex
import Thompson

subsetConstruction :: NFA' a -> DFA' a
subsetConstruction = undefined

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