(Transition(),
                           Node(),
                           FSA(),
                           newTransition,
                           newNode,
                           newFSA) 
\begin{code}
{-# LANGUAGE TypeFamilies,FlexibleContexts,FlexibleInstances #-}
module FiniteStateAutomata(FSA(..), NFA'(..), DFA'(..), epsilon, ppfsa) where

import qualified Data.Map as M
import qualified Data.Set as S

type DFAMap a = M.Map Int (M.Map a Int)
type NFAMap a = M.Map Int (S.Set (Maybe a, Int))

class (Show (Elem m)) => Listable m where
  type Elem m
  toList :: m -> [(Elem m,Int)]

instance (Show a) => Listable (M.Map a Int) where
  type Elem (M.Map a Int) = a 
  toList = M.toList

instance (Show a) => Listable (S.Set (Maybe a, Int)) where
  type Elem (S.Set (Maybe a, Int)) = Maybe a
  toList = S.toList

class (Ord (Alpha f), 
       Show (Alpha f), 
       Show f,
       Show (FSAVal f),
       Listable (FSAVal f)) => FSA f where
  type Alpha f
  type FSAVal f
  alphabet  :: (Ord (Alpha f), Show (Alpha f)) => f -> S.Set (Alpha f)
  accepting :: f -> S.Set Int
  start     :: f -> Int
  trans     :: f -> M.Map Int (FSAVal f)
  states    :: f -> S.Set Int
  states fsa = S.union (S.fromList . M.keys $ (trans fsa)) (accepting fsa)


fsaShow :: (FSA f) => f -> String
fsaShow fsa = "{alphabet=" ++ (show . S.toList . alphabet $ fsa) ++ "," ++
              "states=" ++ (show . S.toList . states $ fsa) ++ "," ++
              "start=" ++ (show . start $ fsa) ++ "," ++
              "accepting=" ++ (show . S.toList . accepting $ fsa) ++ "," ++
              "trans=" ++ (show . map (filter (/= '"')) . showTransitions $ fsa)



pettyPrinter :: (FSA f) => f -> IO ()
pettyPrinter fsa = (putStr $ "alphabet=" ++ (show . S.toList . alphabet $ fsa) ++ "\n" ++
              "states=" ++ (show . S.toList . states $ fsa) ++ "\n" ++
              "start=" ++ (show . start $ fsa) ++ "\n" ++
              "accepting=" ++ (show . S.toList . accepting $ fsa) ++ "\n") >> trans where
  trans = mapM_ (putStrLn . filter (/= '"')) $ showTransitions fsa
  
ppfsa :: (FSA f) => f -> IO ()
ppfsa = pettyPrinter


showTransitions :: (FSA f) => f -> [String]
showTransitions fsa = map showTransition . M.toList . trans $ fsa where
  showTransition (from, ts) = (show from) ++ " :: " ++ (show . map showTransition' . toList $ ts) where
    showTransition' (x, to) = (show x) ++ " -> " ++ (show to)
  
data DFA' a = DFA' {alpha  :: S.Set a,
                    ss     :: DFAMap a,
                    accept :: S.Set Int,
                    st     :: Int}
              
instance (Ord a, Show a) => FSA (DFA' a) where
  type Alpha (DFA' a) = a
  type FSAVal (DFA' a) = (M.Map a Int)
  alphabet = alpha
  accepting = accept
  start = st
  trans = ss

instance (Ord a, Show a) => Show (DFA' a) where
  show dfa = "DFA " ++ (fsaShow dfa)

data NFA' a = NFA' {nalpha  :: S.Set a,
                    nss     :: NFAMap a, 
                    naccept :: S.Set Int,
                    nst     :: Int}
              
epsilon :: Maybe a              
epsilon = Nothing

instance (Ord a, Show a) => FSA (NFA' a) where
  type Alpha (NFA' a) = a
  type FSAVal (NFA' a) = (S.Set (Maybe a, Int))
  alphabet = nalpha
  accepting = naccept
  start = nst
  trans = nss

instance (Ord a, Show a) => Show (NFA' a) where
  show nfa = "NFA " ++ (fsaShow nfa)
  
simpleNFA :: NFA' Char  
simpleNFA = NFA' alpha states accepting start where
  alpha = S.fromList ['a','b']
  states = M.fromList [(0, S.fromList [(Just 'a', 1)]), (1, S.fromList [(Just 'b', 0), (epsilon, 2)])]
  start = 0
  accepting = S.fromList [2]

simpleDFA :: DFA' Char
simpleDFA = DFA' alpha states accepting start where
  alpha = S.fromList ['a','b','c']
  states = M.fromList [(0, M.fromList [('a', 1)]), (1, M.fromList [('b', 0), ('c', 2)])]
  start = 0
  accepting = S.fromList [2]




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