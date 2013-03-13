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

module Hopcroft where

import FiniteStateAutomata
import Regex

import qualified Data.Map as M


data DFA a = DFA {q :: [Int],
              sigma :: [a],
              delta :: M.Map (Int,a) Int,
              q0 :: Int,
              f :: [Int]
              } deriving Show

d = M.fromList [
     ((0,"a"),1),
     ((1,"a"),4),
     ((1,"b"),2),
     ((2,"a"),3),
     ((2,"b"),5),
     ((3,"b"),1),
     ((4,"a"),6),
     ((4,"b"),5),
     ((5,"a"),7),
     ((5,"b"),2),
     ((6,"a"),5),
     ((7,"b"),5)]
        
barbar = DFA 
         [0,1,2,3,4,5,6,7]
         ["a","b"]
         d
         0
         [0,6]

move tuple dfa = M.lookup tuple (delta dfa)

startState fsa = (M.!) (nodes fsa) (start fsa) 

partition fsa = M.partition (isAccepting) (nodes fsa)
accepting = fst . partition

--isConsistent 

--hopcroft fsa = 
--    let (g1,g2) = partition fsa in
    

foo = newFSA 
    (M.fromList [(0, (newNode False [newTransition "a" 1])),
                 (1, (newNode False [newTransition "a" 0]))]) 0

bar = newFSA
    (M.fromList [(0, (newNode False [newTransition "a" 1]))]) 0

min1 = newFSA 
    (M.fromList [(0, (newNode True [newTransition "a" 1])),
                 (1, (newNode False [(newTransition "a" 4), (newTransition "b" 2)])),
                 (2, (newNode False [(newTransition "a" 3), (newTransition "b" 5)])),
                 (3, (newNode False [(newTransition "b" 1)])),
                 (4, (newNode False [(newTransition "a" 6), (newTransition "b" 5)])),
                 (5, (newNode False [(newTransition "a" 7), (newTransition "b" 2)])),
                 (6, (newNode True [(newTransition "a" 5)])),
                 (7, (newNode False [(newTransition "b" 5)]))]) 0 ["a","b"]

min2 = newFSA
       (M.fromList [(1, (newNode True [newTransition "a" 6])),
                    (6, (newNode False [(newTransition "a" 5), (newTransition "b" 7)])),
                    (7, (newNode False [(newTransition "a" 4), (newTransition "b" 6)])),
                    (4, (newNode False [(newTransition "b" 6)])),
                    (5, (newNode False [(newTransition "a" 1), (newTransition "b" 6)]))]) 1 ["a", "b"]

--instance Show a => Show (FSA a) where
--    show (FSA node) = node

-- ->_a i
--instance Show (Transition a) where
--    show t =
--        case t of
--          Transition s i -> "->_" ++ s ++ show i
--          EpsilonT i -> "->_e" ++ show i
        

--instance Show (FSA [Char]) where
--show (FSA m i) = undefined


\end{code}