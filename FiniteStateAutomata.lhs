(Transition(),
                           Node(),
                           FSA(),
                           newTransition,
                           newNode,
                           newFSA) 
\begin{code}
module FiniteStateAutomata where

import qualified Data.Map as M
import qualified Data.Set as S

data Transition a = Transition {getLabel :: a, transition :: Int} 
                  | EpsilonT {transition :: Int} deriving Show

data Node a = Node {isAccepting    :: Bool, 
                    getTransitions :: [Transition a]} deriving Show

data FSA a = FSA {alphabet :: S.Set a,
                  nodes    :: M.Map Int (Node a),
                  start    :: Int
                  } deriving Show

type DFA' a = FSA a
type NFA' a = FSA a

newTransition :: (Eq a, Show a) => a -> Int -> Transition a
newTransition = Transition

newEpsilonT :: (Eq a, Show a) => Int -> Transition a
newEpsilonT = EpsilonT

newNode :: (Eq a, Show a) => Bool -> [Transition a] -> Node a
newNode = Node

newFSA :: (Eq a, Show a) => S.Set a -> M.Map Int (Node a) -> Int -> FSA a
newFSA = FSA

-- NEW -------------------------

data DFA a = DFA {q :: [Int],
              sigma :: [a],
              delta :: M.Map (Int,a) Int,
              q0 :: Int,
              f :: [Int]
              } deriving Show

data Trans = Epsilon | Q Int deriving Show

data NFA a = NFA {nq :: [Int],
              nsigma :: [a],
              ndelta :: M.Map (Trans,a) Int,
              nq0 :: Int,
              nf :: [Int]
              } deriving Show

dfa1 = DFA 
         [0,1,2,3,4,5,6,7]
         ["a","b"]
         d
         0
         [0,6]
         where  
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



\end{code}