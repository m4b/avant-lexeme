\begin{code}
module Algorithms where

import FiniteStateAutomata
import Regex

acceptingNode :: (Eq a, Show a) => Node a
acceptingNode = newNode True []

thompson :: (Eq a, Show a) => Regex a -> FSA a
thompson (Alt r1 r2) = undefined
thompson (Concat r1 r2) = undefined
thompson (Repeat r) = newFSA $ where
  s0 = newNode False [t1,t2]
  s1 = acceptingNode
  s2 = newFSA $ newNode False 
  t1 = 
  
thompson (Term x) = newFSA $ newNode False [t] where
  t = newTransition x acceptingNode
  
thompson Empty = newFSA $ acceptingNode

{--
subset :: FSA a -> FSA a

hopcroft :: FSA a -> FSA a

recognize :: FSA a -> [a] -> Bool

parseDFA :: [Char] -> [Char] -> IO (FSA Char)

parseNFA :: [Char] -> [Char] -> IO (FSA Char)

parseRegex :: [Char] -> [Char] -> IO (Regex Char)

process --???
--}
\end{code}