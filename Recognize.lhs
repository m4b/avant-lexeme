\subsection{Recognize a string for a given DFA}

In this module we test whether a ``string'' from an alphabet for a DFA is accepted by that DFA or not.  It also includes the function tokenize which is utilized by the generated scanner in tokenizing a string for a particular lexical description.

\begin{code}
module Recognize(match,tokenize) where

import FiniteStateAutomata
import qualified Data.Map as M
import qualified Data.Set as S

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


\end{code}

The second function |tokenize| matches a dfa against a string, and produces a token if possible, as required for the final generated scanner.

\begin{code}

tokenize dfa = undefined

\end{code}