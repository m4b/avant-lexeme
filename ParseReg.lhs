\subsection{Parse a Regular Expression}

This module inputs, lexes, and parses a regular expression from a text file.  It uses Hutton's Parselib library.

Parsing is divided into a function for each regular expression.  It handles ascii spaces, newlines, tabs, etc.  I.e., the printable subset of ascii, as required by the spec.

\begin{code}

module ParseReg (getRegex) where

-- alphabet not being used, need to check for membership
-- i suppose
import Alphabet
import Regex

import Parselib

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

getRegex file = 
    case (parse parseRegex') file of
      [] -> error "Could not parse regular expression."
      regex -> (fst . head) regex
                  
-- example
readRegex = do
    source <- readFile "regexp2.txt"
    let regex = getRegex source
    putStrLn $ show regex

\end{code}
