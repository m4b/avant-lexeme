\subsection{Parse a Regular Expression}

This module inputs, lexes, and parses a regular expression from a text file.  It uses Hutton's Parselib library.

Parsing is divided into a function for each regular expression.  It handles ascii spaces, newlines, tabs, etc.  I.e., the printable subset of ascii, as required by the spec.

\begin{code}

module ParseReg (getRegex,parseRegex) where

import Alphabet
import Regex

import Parselib

type Alphabet = [Char]

parseAlt :: Alphabet-> Parser (Regex Char)
parseAlt alphabet = do
  string "|"
  space
  regex <- parseRegex alphabet
  space
  regex' <- parseRegex alphabet
  return (Alt regex regex')

parseConcat :: Alphabet -> Parser (Regex Char)
parseConcat alphabet = do
  string "+"
  space
  regex <- parseRegex alphabet
  space
  regex' <- parseRegex alphabet
  return (Concat regex regex')

parseKleene :: Alphabet -> Parser (Regex Char)
parseKleene alphabet = do
  string "*"
  space
  regex <- parseRegex alphabet
  return (Repeat regex)

parseTerm :: Alphabet -> Parser (Regex Char)
parseTerm alphabet = do
  c <- parseElement
  if not (elem c alphabet) then
      let msg = "Regular expression contains terminal "
                ++ show c 
                ++ " which is not an element of the"
                ++ " alphabet provided." in
       error msg
  else
      return (Term c)

parseRegex :: Alphabet -> Parser (Regex Char)
parseRegex alphabet = do
  space
  parseAlt alphabet +++
   parseConcat alphabet +++
   parseKleene alphabet +++
   parseTerm alphabet

-- takes an alphabet
getRegex :: String -> Alphabet -> Regex Char
getRegex file alphabet = 
    case (parse (parseRegex alphabet)) file of
      [] -> error "Could not parse regular expression."
      regex -> (fst . head) regex
                  
-- example, should error
readRegex1 = do
    source <- readFile "regexp3.txt"
    -- get alphabet before, because alphabet is after
    let alphabet = gotoGetAlphabet source
    let regex = getRegex source alphabet
    putStrLn $ show regex

readRegex file = do
    source <- readFile file
    let alphabet = gotoGetAlphabet source
    let regex = getRegex source alphabet
    putStrLn $ show regex


\end{code}
