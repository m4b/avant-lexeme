\subsection{Parse an NFA}

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module ParseNFA where

import Alphabet
import Data.Functor
import FiniteStateAutomata
import Text.Parsec


parseNFA' :: String -> NFA' Char
parseNFA' s = undefined

nfa' :: Stream s m Char => ParsecT s u m (NFA' Char)
nfa' = do
  string "nfa" >> newline
  string "end;" >> newline
  return undefined

  
  
parseStates :: Stream s m Char => ParsecT s u m [String]
parseStates = do
  string "states" >> newline
  reverse <$> parseStates' [] where
    parseStates' acc = do
      sOrT <- parseStringOrTerm "end;" (many1 (noneOf " \n"))
      newline
      case sOrT of
        Left ter -> return acc
        Right st -> parseStates' (st : acc)
    
parseStringOrTerm :: Stream s m Char => String -> 
                                        ParsecT s u m String -> 
                                        ParsecT s u m (Either String String)
parseStringOrTerm term s = do
   str <- (try $ string term) <|> s
   return $ if str == term then Left str else Right str
   
parseAlphabet :: Stream s m Char => ParsecT s u m [Alphabet]
parseAlphabet = do
  tok <- AlphabetToken <$ string "alphabet"
  newline
  syms <- sym `sepBy1` (char ' ')
  newline
  endTok <- EndToken <$ string "end"
  return $ tok : (syms ++ [endTok])

sym :: Stream s m Char => ParsecT s u m Alphabet
sym = char '\'' >>  Symbol <$> anyChar
\end{code}