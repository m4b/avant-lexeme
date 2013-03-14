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



\end{code}