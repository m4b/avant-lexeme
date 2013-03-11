\begin{code}
module FiniteStateAutomata(Transition(),
                           Node(),
                           FSA(),
                           newTransition,
                           newNode,
                           newFSA) where

import qualified Data.HashMap.Strict as M

data Transition a = Transition {getLabel :: a, transition :: Int} 
                  | EpsilonT {transition :: Int}

data Node a = Node {isAccepting :: Bool, getTransitions :: [Transition a]}

data FSA a = FSA {nodes :: M.HashMap Int (Node a),
                  start :: Int }

newTransition :: (Eq a, Show a) => a -> Int -> Transition a
newTransition = Transition

newEpsilonT :: (Eq a, Show a) => Int -> Transition a
newEpsilonT = EpsilonT

newNode :: (Eq a, Show a) => Bool -> [Transition a] -> Node a
newNode = Node

newFSA :: (Eq a, Show a) => M.HashMap Int (Node a) -> Int -> FSA a
newFSA = FSA
\end{code}