\section{Hopcroft's Algorithm}

In this module we provide our solution for minimizing a given deterministic finite state automaton.

The pseudocode for this algorithm from \url{https://en.wikipedia.org/wiki/DFA_minimization} is as follows:

\begin{verbatim}

P := {F, Q \ F};
W := {F};
while (W is not empty) do
     choose and remove a set A from W
     for each c in ∑ do
          let X be the set of states for which a transition on c leads to a state in A
          for each set Y in P for which X ∩ Y is nonempty do
               replace Y in P by the two sets X ∩ Y and Y \ X
               if Y is in W
                    replace Y in W by the same two sets
               else
                    if |X ∩ Y| <= |Y \ X|
                         add X ∩ Y to W
                    else
                         add Y \ X to W
          end;
     end;
end;

\end{verbatim}



\begin{code}

module Hopcroft(hopcroft) where

import FiniteStateAutomata
import Regex

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

hopcroft :: (Ord a, Show a) => DFA' a -> DFA' a
hopcroft = undefined

-- | Removes all unreachable states in a DFA'
dropUnreachable :: (Ord a, Show a) => DFA' a -> DFA' a
dropUnreachable dfa = dropUnreachable' set set dfa where 
  set = S.singleton $ start dfa

dropUnreachable' :: (Ord a, Show a) => S.Set Int -> S.Set Int -> DFA' a -> DFA' a
dropUnreachable' reachable_states new_states dfa = if done then dfa' else recurse where
  reachable'        = S.unions . S.toList . S.map (reachable dfa) $ new_states
  new_states'       = S.difference reachable' reachable_states
  reachable_states' = S.union reachable_states new_states'
  recurse           = dropUnreachable' reachable_states' new_states' dfa 
  dfa'              = updateDFA dfa reachable_states'
  done              = new_states' == S.empty

updateDFA :: (Ord a, Show a) => DFA' a -> S.Set Int -> DFA' a
updateDFA dfa reachable_states = DFA' alphabet' trans' accept' start' where
  unreachable_states = S.difference (states dfa) reachable_states
  accept'            = S.difference (accepting dfa) unreachable_states
  alphabet'          = alphabet dfa
  start'             = start dfa
  trans'             = M.filterWithKey removeKey (trans dfa)
  removeKey k _      = S.member k reachable_states
  

reachable :: (Ord a, Show a) => DFA' a -> Int -> S.Set Int
reachable fsa state = S.fromList ns where
  trans'    = M.lookup state (trans fsa)
  ns        = if isNothing trans' then [] else ns'
  ns'       = M.elems . fromJust $ trans'

-- A test DFA that has several unreachable states: [3,4,5,6]
testDFA :: DFA' Char
testDFA = DFA' alpha' ss' accept' st' where
  alpha'  = S.fromList "ab"
  ss'     = M.fromList [(0, trans0), (1, trans1), (2, trans2), (3, trans3)]
  trans0  = M.fromList [('a', 1), ('b', 2)]
  trans1  = M.empty
  trans2  = M.empty
  trans3  = M.fromList [('a', 4), ('b', 5)]  
  accept' = S.fromList [1,2,3,6]
  st'     = 0

-- Tests the removal of unreachable states
testDroppable :: Bool
testDroppable = alphabet' && states' && start' && accepting' where
  alphabet'  = (alphabet dfa) == (S.fromList "ab")
  states'    = (states dfa) == (S.fromList [0,1,2])
  start'     = (start dfa) == 0
  accepting' = (accepting dfa) == (S.fromList [1,2])
  dfa        = dropUnreachable testDFA


-- A test DFA that can be reduced to a single node with two edges
-- it recognizes strings of the language (a|b)*
testDFA' :: DFA' Char
testDFA' = DFA' alpha' ss' accept' st' where
  alpha'  = S.fromList "ab"
  ss'     = M.fromList [(0, trans'), (1, trans'), (2, trans')]
  trans'  = M.fromList [('a', 1), ('b', 2)]
  accept' = S.fromList [0,1,2]
  st'     = 0
  
-- Tests that hopcroft reduces testDFA' to a minimal dfa  
testHopcroft :: Bool
testHopcroft = alphabet' && states' && start' && accepting' && trans' where
  alphabet'  = (alphabet dfa) == (S.fromList "ab")
  states'    = (states dfa) == (S.fromList [0])
  start'     = (start dfa) == 0
  accepting' = (accepting dfa) == (S.fromList [0])
  trans'     = (trans dfa) == (M.fromList [(0, trans0)])
  trans0     = M.fromList [('a', 0), ('b', 0)]
  dfa        = hopcroft testDFA'
  
\end{code}