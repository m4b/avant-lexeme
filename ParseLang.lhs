\subsection{Parse a Lexical Description of a Programming Language}

\begin{code}

module ParseLang where

import Parselib
import Regex
import ParseReg
import FiniteStateAutomata

import Data.Char (isSpace)

data LexDesc = 
    Language String |
    Alphabet String

parseElement :: Parser Char
parseElement = do
  space
  char '\''
  c <- alphanum +++ char ' ' +++ char '\n' +++ char '\t'
  return c

parseAlphabet :: Parser [Char]
parseAlphabet = do
  space
  string "alphabet"
  alphabet <- many parseElement
  space
  string "end;"
  return alphabet

parseLangIdentifier :: Parser String
parseLangIdentifier = do
  ident <- many $ sat (not . isSpace)
  return ident

--parseLang :: Parser LangDesc
parseLang = do
  space
  string "language"
  space
  identifier <- parseLangIdentifier
  alphabet <- parseAlphabet

  return identifier

readLang = do
  source <- readFile "tests/lexdesc1.txt"
  let x = (parse parseLang) source
  putStrLn $ show x

  {-

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


--parseLang :: Parser 
--parseLang 
-}

\end{code}