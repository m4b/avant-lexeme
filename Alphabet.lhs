\section{Alphabet}

This module provides functions for lexing and parsing alphabets found in input files, in addition to an alphabet token data structure.

\begin{code}

module Alphabet where

import Data.List

data Alphabet =
              Symbol Char |
              AlphabetToken |
              EndToken deriving (Show, Eq)

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

parseAlphabet [] = []
parseAlphabet (AlphabetToken:ts) = 
    parseAlphabet ts
parseAlphabet (Symbol c:ts) = 
    c:parseAlphabet ts
parseAlphabet (EndToken:ts) =
    []

getAlphabet = parseAlphabet . scanAlphabet . gotoAlphabet

\end{code}