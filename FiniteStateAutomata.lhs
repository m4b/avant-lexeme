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
                  | EpsilonT {transition :: Int}

instance (Show a) => Show (Transition a) where
  show (Transition lbl trans) = (show lbl) ++ " -> " ++ (show trans)
  show (EpsilonT trans)       = "eps "     ++ " -> " ++ (show trans)

data Node a = Node {isAccepting    :: Bool, 
                    getTransitions :: [Transition a]}

data FSA a = FSA {alphabet :: S.Set a,
                  states   :: M.Map Int (Node a),
                  start    :: Int
                  }
             
instance (Show a, Eq a) => Show (FSA a) where
  show fsa@(FSA as ns start) = "FSA {" ++
                           "alphabet=" ++ (show . S.toList $ as) ++ ", " ++
                           "states=" ++ (show . M.keys $ ns) ++ ", " ++
                           "start=" ++ (show start) ++ ", " ++
                           "trans=" ++ (filter (/= '"') . show . showTransitions $ fsa)
                           
showTransitions :: (Show a) => FSA a -> [String]
showTransitions (FSA _ ss _) = map showTransition (M.toList ss) where
  showTransition (x,Node acc ts) = accStar ++ (show x) ++ " :: " ++ (show ts)
    where accStar = if acc then "*" else ""

--For internal use only
pettyPrintFSA :: (Show a, Eq a) => FSA a -> IO ()
pettyPrintFSA fsa@(FSA as ns start) = 
  (putStr $ "FSA \n" ++
             "alphabet=" ++ (show . S.toList $ as) ++ " \n" ++
             "states=" ++ (show . M.keys $ ns) ++ " \n" ++
             "start=" ++ (show start) ++ "\n" ++
             "transitions\n") >> prettyTrans
  where prettyTrans = mapM_ putStrLn . showTransitions $ fsa

ppfsa :: (Show a, Eq a) => FSA a -> IO ()
ppfsa = pettyPrintFSA

type DFA' a = FSA a
type NFA' a = FSA a

simpleFSA = FSA alpha states start where
  alpha = S.fromList ['a','b']
  states = M.fromList [(0, s0), (1, s1), (2, s2)]
  start = 0
  s0 = Node False [Transition 'a' 1]
  s1 = Node False [Transition 'b' 0, EpsilonT 2]
  s2 = Node True []

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