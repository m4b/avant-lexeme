\section{Alphabet}

This module provides functions for lexing and parsing alphabets found in input files, in addition to an alphabet token data structure.

\begin{code}

module Alphabet(parseElement, parseAlphabet, getAlphabet, gotoGetAlphabet) where

import Data.List
import Parselib
import GHC.Unicode (isPrint)
import Data.Char (ord)

\end{code}

**TODO**

A formal description of an alphabet is:
\begin{verbatim}

alphabet -> alphabet_keyword elements end_keyword
elements -> 'subset_ascii
elements -> elements
subset_ascii -> 
             a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|
             w|x|y|z|A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|
             T|U|V|W|X|Y|Z|\n|\t|\r| |

\end{verbatim}

The Haskell version of this might be written as follows:

\begin{code}

data Alphabet =
              AlphabetToken | 
              Symbol Char |
              EndToken deriving (Show, Eq)

\end{code}

The remaining functions find an alphabet in a file, tokenize, and generate a list of the symbols in the alphabet.

\begin{code}

gotoAlphabet [] =  []
gotoAlphabet cs | isPrefixOf "alphabet" cs = cs
gotoAlphabet (c:cs) =  gotoAlphabet cs

scanAlphabet [] = []
scanAlphabet ('a':'l':'p':'h':'a':'b':'e':'t':cs) = 
    AlphabetToken:scanAlphabet cs
scanAlphabet ('\'':c:cs) = 
    Symbol c:scanAlphabet cs
scanAlphabet ('e':'n':'d':cs) = 
    [EndToken]
scanAlphabet (_:cs) = 
    scanAlphabet cs

parseAlphabet' [] = []
parseAlphabet' (AlphabetToken:ts) = 
    parseAlphabet' ts
parseAlphabet' (Symbol c:ts) = 
    c:parseAlphabet' ts
parseAlphabet' (EndToken:ts) =
    []

getAlphabet' = parseAlphabet' . scanAlphabet . gotoAlphabet

-- this was not a fun bug to track down
parseEscapedChar = 
      do {string "\\n"; return '\n'}
  +++ do {string "\\t"; return '\t'}
  +++ do {string "\\v"; return '\v'}
  +++ do {string "\\r"; return '\r'}
  +++ do {string "\\b"; return '\b'}
  +++ do {string "\\a"; return '\a'}
  +++ do {string "\\f"; return '\f'}
  +++ do {string "\\" ; return '\\'}

printablePlus char = 
    let ascii = ord char in
    (ascii >= 7 && ascii <= 13 ) 
    || (ascii >= 32 && ascii <= 126)

parseElement :: Parser Char
parseElement = do
  space
  char '\''
  c <- 
      parseEscapedChar +++
      sat printablePlus
{-        char '.' +++ char '+'
       +++ char '-' +++ char '*'
       +++ char '/'  -- +++ char '\\' <-- fundamentally evil
       +++ char '0' +++ char '1'
       +++ char '2' +++ char '3'
       +++ char '4' +++ char '5'
       +++ char '6' +++ char '7'
       +++ char '8' +++ char '9'
       +++ char ' ' +++ char '\b'
       +++ char '\n' +++ char '\t'
-}

  return c

parseAlphabet :: Parser [Char]
parseAlphabet = do
  space
  string "alphabet"
  alphabet <- many parseElement
  space
  string "end;" +++ string "end" -- need because test file
  return alphabet

getAlphabet file = 
    case (parse parseAlphabet) file of
      [] -> error "Alphabet is empty (no alphabet provided)."
      regex -> (fst . head) regex

-- to skip beginning contents and read alphabet
gotoGetAlphabet file = 
    case (parse parseAlphabet) (gotoAlphabet file) of
      [] -> error "Alphabet is empty (no alphabet provided)."
      regex -> (fst . head) regex


\end{code}