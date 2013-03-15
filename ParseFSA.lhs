\subsection{Parse an FSA}

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module ParseFSA(parseNFA,parseDFA) where

import Data.Functor
import qualified Data.Set as S
import qualified Data.Map as M
import FiniteStateAutomata
import Text.Parsec

data Transition = NFAT{fromState :: String, 
                       symbols :: [Char],
                       toState :: String}
                | DFAT{fromState :: String,
                       symbol :: Char,
                       toState :: String}
                       
instance Show Transition where
  show (NFAT f ss t) = "NFAT " ++ f ++ " " ++ (show ss) ++ " --> " ++ t
  show (DFAT f s t) = "DFAT " ++ f ++ " " ++ (show s) ++ " --> " ++ t
                       
data Description = Description {states' :: [String],
                                startState :: String,
                                acceptStates :: [String],
                                trans' :: [Transition]} deriving Show

parseNFA :: [Char] -> String -> NFA' Char
parseNFA = parseFSA "nfa" (NFA') (toNFAMap)
  
parseDFA :: [Char] -> String -> DFA' Char
parseDFA = parseFSA "dfa" (DFA') (toDFAMap)
  
parseFSA typ constr toMap alpha s = 
  case parse (description typ isNFA) "Syntax Error" s of
    Left er -> error . show $ er
    Right desc -> convertToFSA alpha desc constr toMap
  where isNFA = if typ == "nfa" then True else False
  
convertToFSA :: FSA f => [Alpha f] -> Description -> 
                (S.Set (Alpha f) -> M.Map Int (FSAVal f) -> S.Set Int -> Int -> f) -> 
                (M.Map String Int -> [Transition] -> M.Map Int (FSAVal f)) ->
                f
convertToFSA alpha desc const toMap = const alphabet nfaMap accpting start where
  normal = M.fromList $ zip (states' desc) [0..]
  alphabet = S.fromList alpha
  nfaMap = (toMap normal) . trans' $ desc
  accpting = S.fromList . map (normal M.!) . acceptStates $ desc
  start = normal M.! (startState desc)

toNFAMap :: M.Map String Int -> [Transition] -> NFAMap Char
toNFAMap m ts = M.fromList . map convert . M.toList . go ts $ M.empty where
  convert (s,es) = (m M.! s, S.fromList . map (\(c,s2) -> (c,m M.! s2)) $ es)
  toM "" = [Nothing]
  toM s = map Just s
  go [] acc = acc
  go ((NFAT f syms t):ts) acc = case M.lookup f acc of
    Nothing -> go ts $ M.insert f (zip (toM syms) (repeat t)) acc
    Just es -> go ts $ M.insert f ((zip (toM syms) (repeat t)) ++ es) acc
      
toDFAMap :: M.Map String Int -> [Transition] -> DFAMap Char
toDFAMap m ts = M.fromList . map convert . M.toList . go ts $ M.empty where
  convert (s,es) = (m M.! s, M.fromList . map (\(c,s2) -> (c,m M.! s2)) $ es)
  go [] acc = acc
  go ((DFAT f symb t):ts) acc = case M.lookup f acc of
    Nothing -> go ts $ M.insert f [(symb,t)] acc
    Just es -> go ts $ M.insert f ((symb,t) : es) acc

description :: Stream s m Char => 
        String ->
        Bool ->
        ParsecT s u m Description
description keyword isNFA = do
  spaces >> string keyword >> spaces
  stats <- statelist "states" "end;" identifier
  initState <- initialState "initial"
  acceptStates <- statelist "accept" "end;" identifier
  trans <- statelist "transitions" "end;" (transition isNFA)
  --alphabet <- parseAlphabet
  return $ Description stats initState acceptStates trans
  
transition :: Stream s m Char => 
              Bool ->
              ParsecT s u m Transition
transition isNFA = do
  from <- identifier
  syms <- option [] symbolList
  string "-->" >> spaces
  to <- identifier
  return $ case isNFA of
    True -> NFAT from syms to
    False -> DFAT from (head syms) to


initialState :: Stream s m Char => String -> ParsecT s u m String
initialState keyword = string keyword >> spaces >> identifier
    
parseStringOrTerm :: Stream s m Char => String -> 
                                        ParsecT s u m a -> 
                                        ParsecT s u m (Either String a)
parseStringOrTerm term s = do
   ter <- try $ optionMaybe $ string term
   case ter of
     Just t -> return $ Left t
     Nothing -> Right <$> s
   --str <- (try $ string term) <|> s
   --return $ if str == term then Left str else Right str
   
{--
parseAlphabet :: Stream s m Char => ParsecT s u m [Char]
parseAlphabet = do
  string "alphabet" >> spaces
  syms <- symbolList
  string "end"
  return syms
--}
  
statelist :: Stream s m Char => 
             String -> 
             String -> 
             ParsecT s u m a ->
             ParsecT s u m [a]
statelist startTok endTok elem = do
  string startTok >> spaces
  reverse <$> parseSets' [] where
    parseSets' acc = do
      sOrT <- parseStringOrTerm endTok elem
      spaces
      case sOrT of
        Left _ -> return acc
        Right str -> parseSets' $ str : acc
        
identifier :: Stream s m Char => ParsecT s u m String
identifier = do
  i <- many1 alphaNum
  spaces
  return i

sym :: Stream s m Char => ParsecT s u m Char
sym = do
  char '\'' 
  c <- anyChar
  case c of
    '\\' -> do
      c2 <- anyChar
      return $ read $ "'\\" ++ [c2] ++ "'"
    _ -> return c

symbolList :: Stream s m Char => ParsecT s u m [Char]
symbolList = sym `sepEndBy1` (spaces)
\end{code}