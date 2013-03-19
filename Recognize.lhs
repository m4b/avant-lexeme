\subsection{Recognize a string for a given DFA}

In this module we test whether a ``string'' from an alphabet for a DFA is accepted by that DFA or not.  It also includes the function tokenize which is utilized by the generated scanner in tokenizing a string for a particular lexical description.

\begin{code}
module Recognize(match, match',tokenize) where

import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

\end{code}

The function |match| takes an \(\alpha\) DFA and list of \(\alpha\) as input (a ``string'' in the language of that DFA), and returns true if the string is accepted by that DFA, and false otherwise.  It is fairly straightforward.

\begin{code}

match :: (Ord a, Show a) => DFA' a -> [a] -> Bool
match dfa = match' dfa (start dfa) where
  match' dfa curr [] = S.member curr (accepting dfa)
  match' dfa curr (c:cs) = 
   let labelMap = M.lookup curr (trans dfa) in 
    case labelMap of
      Nothing -> False
      Just map -> let labels = M.lookup c map in 
        case labels of
          Nothing -> False
          Just next -> match' dfa next cs



match' :: (Ord a, Show a) => DFA' a -> a -> (Bool, Maybe (DFA' a))
match' dfa@(DFA' alpha ss accept st) sym = (isAccepting, dfa') where
  curr = start dfa
  lookup = M.lookup curr (trans dfa)
  nextStart = if isNothing lookup then Nothing else M.lookup sym (fromJust lookup)
  dfa' = if isNothing nextStart then Nothing else Just (DFA' alpha ss accept (fromJust nextStart))
  isAccepting = if isNothing nextStart then False else S.member (fromJust nextStart) (accepting dfa)

\end{code}

The second function |tokenize| matches a dfa against a string, and produces a token if possible, as required for the final generated scanner.

\begin{code}

type Match a = (Int, [a], [a])

tokenize :: (Ord a, Show a) => [a] -> DFA' a -> (Int, [a], [a])
tokenize toScan dfa = tokenize' (0, [], toScan) (0, [], toScan) dfa toScan where
  tokenize' :: (Ord a, Show a) => Match a -> Match a -> DFA' a -> [a] -> (Int, [a], [a])
  tokenize' (consumed, token, rem) _ _ [] = (consumed, reverse token, rem)
  tokenize' currentMatch candidate dfa (x:xs) = if done then finalize currentMatch else recurse where
    done = isNothing dfa'
    (consumed, acc, _) = candidate
    (matches, dfa') = match' dfa x
    acc' = (x:acc)
    consumed' = consumed + 1
    candidate' = (consumed', acc', xs)
    currentMatch' = if matches then candidate' else currentMatch
    recurse = tokenize' currentMatch' candidate' (fromJust dfa') xs
    finalize (c, t, m) = (c, reverse t, m)
    
\end{code}