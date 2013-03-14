\subsection{Parse a Regular Expression}

This module inputs, lexes, and parses a regular expression from a text file.

\begin{code}

module ParseReg where

-- alphabet not being used, need to check for membership
-- i suppose
import Alphabet
import Regex
import Data.List
import Data.Char (isSpace)

import Parselib

data Tokens = 
            AltToken |
            ConcatToken |
            KleeneToken | 
            TermToken Char |
            EmptyToken deriving (Show, Eq)


tokenize [] = []
tokenize ('|':cs) = AltToken:tokenize cs
tokenize ('+':cs) = ConcatToken:tokenize cs
tokenize ('*':cs) = KleeneToken:tokenize cs
tokenize ('\'':' ':cs) = EmptyToken:tokenize cs
tokenize ('\'':c:cs) = TermToken c:tokenize cs
tokenize (cs) | isPrefixOf "alphabet" cs = []
tokenize (c:cs) | isSpace c = tokenize cs
tokenize (c:cs) = error ("unknown character" 
               ++ show c ++ " in regular expression")

parseAlt :: Parser (Regex Char)
parseAlt = do
  string "|"
  space
  regex <- parseRegex'
  space
  regex' <- parseRegex'
  return (Alt regex regex')

parseConcat :: Parser (Regex Char)
parseConcat = do
  string "+"
  space
  regex <- parseRegex'
  space
  regex' <- parseRegex'
  return (Concat regex regex')

parseKleene :: Parser (Regex Char)
parseKleene = do
  string "*"
  space
  regex <- parseRegex'
  return (Repeat regex)

parseTerm :: Parser (Regex Char)
parseTerm = do
  char '\''
  c <- alphanum +++ char ' ' +++ char '\n' +++ char '\t'
  return (Term c)

parseRegex' :: Parser (Regex Char)
parseRegex' = do
  space
  parseAlt +++ parseConcat +++ parseKleene +++ parseTerm

getRegex' = parse parseRegex'
                  
-- example
readRegex = do
    source <- readFile "regexp2.txt"
    let regex = getRegex' source
    putStrLn $ show regex

\end{code}
