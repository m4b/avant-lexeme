\begin{code}
module FiniteStateAutomata(Transition(),
                           Node(),
                           FSA(),
                           newTransition,
                           newNode,
                           newFSA) where

data Transition a = Transition {getLabel :: a, getNode :: Node a} 

data Node a = Node {isAccepting :: Bool, getTransitions :: [Transition a]}

data FSA a = FSA {getStart :: Node a}

newTransition :: (Eq a, Show a) => a -> Node a -> Transition a
newTransition = Transition

newNode :: (Eq a, Show a) => Bool -> [Transition a] -> Node a
newNode = Node

newFSA :: (Eq a, Show a) => Node a -> FSA a
newFSA = FSA
\end{code}