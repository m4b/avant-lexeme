\subsection{Recognize a string for a given DFA}

In this module we test a string with a given DFA and determine whether the DFA accepts the string or not.

\begin{code}
module Recognize(match) where

import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S

\end{code}

The function |match| takes a DFA and a string as input, and returns whether that string is accepted by that DFA.  It is fairly straightforward.

\begin{code}

match :: (Ord a, Show a) => DFA' a -> [a] -> Bool
match dfa = match' dfa (start dfa) where
  match' dfa curr [] = S.member curr (accepting dfa)
  match' dfa curr (c:cs) = let labelMap = M.lookup curr (trans dfa) in 
    case labelMap of
      Nothing -> False
      Just map -> let labels = M.lookup c map in 
        case labels of
          Nothing -> False
          Just next -> match' dfa next cs


\end{code}