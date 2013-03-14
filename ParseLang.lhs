\subsection{Parse a Lexical Description of a Programming Language}

\begin{code}

{-# LANGUAGE TypeFamilies,
  FlexibleContexts,FlexibleInstances #-}

module ParseLang where

import Parselib
import Regex
import ParseReg
import FiniteStateAutomata
import Alphabet

import Data.Char (isSpace)
import Data.List (intersperse)

data LexDesc = 
    Language String |
    Alphabet [Char] 

type Identifier = String
data Relevance = Relevant | Irrelevant | Discard deriving Show
data Class = Class {
      name :: Identifier,
      regex :: Regex Char,
      relevance :: Relevance} deriving Show

data Desc = Desc {
    language :: String,
    alphabet :: [Char],
    classes :: [Class]
    } deriving Show

showAlphabet a = intersperse '\'' a

{-instance Show (Desc -> Identifier -> (Regex Char) -> Relevance) where
    show Desc l a c = 
        "language: " ++ show l ++ "\n"
        "alphabet: " ++ showAlphabet a ++ "\nhello"
  -}      

parseLangIdentifier :: Parser String
parseLangIdentifier = do
  ident <- many $ sat (not . isSpace)
  return ident

parseRelevance :: Parser Relevance
parseRelevance = 
    do {string "relevant"   ; return Relevant} +++
    do {string "irrelevant" ; return Irrelevant} +++
    do {string "discard"    ; return Discard}

parseClass :: [Char] -> Parser Class  
parseClass alphabet = do
  space
  string "class"
  space
  name <- parseLangIdentifier
  space 
  string "is"
  regex <- parseRegex alphabet
  space
  relevance <- parseRelevance
  space
  string "end;"
  return $ Class name regex relevance

parseLang :: Parser Desc
parseLang = do
  space
  string "language"
  space
  language <- parseLangIdentifier
  alphabet <- parseAlphabet
  classes <- many $ parseClass alphabet
  space
  string "end;"
  return (Desc language alphabet classes)

-- example
readLang1 = do
  source <- readFile "tests/lexdesc1.txt"
  let x = (parse parseLang) source
  putStrLn $ show x

readLang file = do
  source <- readFile file
  let x = (parse parseLang) source
  putStrLn $ show x


\end{code}