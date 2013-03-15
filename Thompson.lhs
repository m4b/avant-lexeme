\subsection{Thompson's Algorithm}

In this module we provide our solution for converting a regular expression to an NFA.

\begin{code}

module Thompson (thompson) where

import Prelude hiding(concat)
import qualified Data.Set as S
import qualified Data.Map as M
import FiniteStateAutomata(FSA(trans), NFA'(..), epsilon)
import Regex

\end{code}

The function |thompson| returns the result of converting a regular expression to a non-deterministic finite state automaton.  It uses Thompson's algorithm for doing so.

\begin{code}

thompson :: (Ord a, Show a) => Regex a -> NFA' a
thompson = fst . thompson' 0

thompson' :: (Ord a, Show a) =>
           Int -> Regex a -> (NFA' a, Int)
thompson' lab (Alt r1 r2) = union lab'' fsa fsa' where
  (fsa, lab') = thompson' lab r1
  (fsa', lab'') = thompson' lab' r2
thompson' lab (Concat r1 r2) = concat lab'' fsa fsa' where
  (fsa, lab') = thompson' lab r1
  (fsa', lab'') = thompson' lab' r2
thompson' lab (Kleene r1) = mrKleene lab' fsa where
  (fsa, lab') = thompson' lab r1
thompson' lab (Term x) = symbol lab x
thompson' lab Empty = expression lab

\end{code}

The following functions individually convert particular regular expressions to their NFA equivalents.  For example, |concat| takes two regular expressions, say \emph a and \emph b, and returns an NFA where the NFA corresponding to \emph a's accepting states are now transitions to the NFA corresponding to \emph b's start state.

The other functions perform similar operations, according to the algorithm.

\begin{code}
expression :: (Ord a, Show a) => Int -> (NFA' a, Int)
expression label = (fsa, label+2) where
  fsa = NFA' S.empty (M.fromList [n1])
        (S.singleton (label+1)) label
  n1 = (label, S.singleton (epsilon, (label+1)))
        

symbol :: (Ord a, Show a) => Int -> a -> (NFA' a, Int)
symbol label sym = (fsa, label+2) where
  fsa = NFA' (S.singleton sym)
        (uncurry M.singleton n1) (S.singleton (label+1)) label
  n1 = (label, S.singleton (Just sym, label+1))
  

union :: (Ord a, Show a) =>
         Int -> NFA' a -> NFA' a -> (NFA' a, Int)
union label nfa0 nfa1 = (fsa, label+2) where
  (NFA' a0 m0 as0 st0) = updateAccepting [(label+1)] nfa0
  (NFA' a1 m1 as1 st1) = updateAccepting [(label+1)] nfa1
  fsa = NFA' alpha newMap (S.singleton (label+1)) label
  alpha = S.union a0 a1
  newMap = M.unions [m0, m1, epsilonEdges]
  epsilonEdges = 
      M.singleton label 
           (S.fromList [ (epsilon, st0),  (epsilon, st1) ])

concat :: (Ord a, Show a) => 
          Int -> NFA' a -> NFA' a -> (NFA' a, Int)
concat label fsa0@(NFA' s0 m0 as0 st0) (NFA' s1 m1 as1 st1) =
    (fsa, label) where
  fsa = NFA' (S.union s0 s1) (M.union updated m1) as1 st0
  updated = trans $ updateAccepting [st1] fsa0  
  
mrKleene :: (Ord a, Show a) => Int -> NFA' a -> (NFA' a, Int)
mrKleene label nfa@(NFA' a _ as st) = (fsa, label+2) where
  (NFA' _ m _ _) = updateAccepting [st, (label+1)] nfa
  fsa = NFA' a m' (S.singleton (label+1)) label
  m' = M.union m epsilons
  epsilons = 
      M.singleton label 
           (S.fromList [(epsilon, (label+1)), (epsilon, st)])
  epsilons' = M.fromList . map func . S.toList $ as
  func x = (x, S.singleton (epsilon, label+1))

updateAccepting :: (Ord a) => [Int] -> NFA' a -> NFA' a
updateAccepting is nfa@(NFA' a ts as st) = 
    NFA' a newTrans (S.empty) st where
        newTrans = M.union ts nts
        nts = M.fromList . map func . S.toList $ as
        func x = 
            (x, S.fromList . map (\i -> (epsilon, i)) $ is)
                                           
\end{code}