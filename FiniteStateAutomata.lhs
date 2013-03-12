\begin{code}
module FiniteStateAutomata(Transition(),
                           Node(),
                           FSA(),
                           newTransition,
                           newNode,
                           newFSA) where

import qualified Data.HashMap.Lazy as M

data Transition a = Transition {getLabel :: a, getNode :: Node a} 
                  | EpsilonT {getNode :: Node a}

data Node a = Node {getID :: Int, isAccepting :: Bool, getTransitions :: [Transition a]}

data FSA a = FSA {getStart :: Node a}

instance (Show a) => Show (Node a) where
  show (Node id acc trans) = (show id) ++ " " ++ (show acc) ++ " " ++ (show trans)

instance (Show a) => Show (Transition a) where
  show (Transition lbl n) = (show lbl) ++ " -> " ++ (show (getID n))
  show (EpsilonT n) = "eps -> " ++ (show (getID n))
  
instance (Show a) => Show (FSA a) where
  show fsa = show $ findNodes fsa

type NodeMap a = M.HashMap Int (Node a)

findNodes :: FSA a -> [Node a]
findNodes (FSA s) = s:(findNodes' (getTransitions s) (M.singleton (getID s) s))

findNodes' :: [Transition a] -> NodeMap a -> [Node a]
findNodes' [] _ = []
findNodes' (t:ts) m = let node = getNode t in
  case (M.lookup (getID node) m) of
    Just _ -> findNodes' ts m
    Nothing -> node:(findNodes' (ts ++ (getTransitions node)) (M.insert (getID node) node m))

testFSA :: FSA Char
testFSA = FSA s0 where
  s0 = Node 0 False [Transition 'a' s1]
  s1 = Node 1 False [Transition 'b' s0, EpsilonT s2]
  s2 = Node 2 True []
  
newTransition :: (Eq a, Show a) => a -> Node a -> Transition a
newTransition = Transition

newEpsilonT :: (Eq a, Show a) => Node a -> Transition a
newEpsilonT = EpsilonT

newNode :: (Eq a, Show a) => Int -> Bool -> [Transition a] -> Node a
newNode = Node

newFSA :: (Eq a, Show a) => Node a -> FSA a
newFSA = FSA
\end{code}