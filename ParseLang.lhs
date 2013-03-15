\subsection{Parse a Lexical Description of a Programming Language}

In this module we parse a lexical description of a language, and prepare it for parsing with respect to our previous data structures and algorithms.

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
data Relevance = Relevant | Irrelevant | Discard 
                 
instance Show Relevance where
    show (Relevant) = "relevant"
    show (Irrelevant) = "irrelevant"
    show (Discard) = "discard"

data Class = Class {
      name :: Identifier,
      regex :: Regex Char,
      relevance :: Relevance}

instance Show Class where
    show c = "class " ++ name c ++ " " ++
             show (regex c) ++  " " ++
             show (relevance c) ++
             " end;"

data Desc = Desc {
    language :: String,
    symbols :: [Char],
    classes :: [Class]
    } 

showAlphabet a = "'" ++ (intersperse '\'' a)

instance Show Desc where
    show desc = 
         "language: " ++ language desc ++ "\n" ++
         "alphabet: " ++
         showAlphabet (symbols desc) ++
         " end;" ++ "\n" ++
         "classes: "  ++ "\n" ++ 
         unlines (map show (classes desc)) ++
         " end;"

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

getLang :: FilePath -> IO Desc
getLang file = do
  source <- readFile file
  case (parse parseLang) source of
    [] -> error "Could not parse lexical description."
    regex -> return $ (fst . head) regex

-- example
readLang1 = do
  source <- readFile "tests/lexdesc3.txt"
  let x = (parse parseLang) source
  putStrLn $ show x

readLang file = do
  source <- readFile file
  let x = (parse parseLang) source
  putStrLn $ show x



\end{code}