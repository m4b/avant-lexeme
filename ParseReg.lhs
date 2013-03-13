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

-- yea, I know, leave me alone
parse :: [Tokens] -> Maybe (Regex Char,[Tokens])
parse (AltToken:tokens) = 
    case parse tokens of
      Just (regex,tokens') -> 
          case parse tokens' of
            Just (regex',tokens'') -> 
                Just ((Alt regex regex'),tokens'')
            _ -> error "alternation missing 2nd operand"
      _ -> error "alternation missing 1st operand"
parse (ConcatToken:tokens) = 
    case parse tokens of
      Just (regex,tokens') -> 
          case parse tokens' of
            Just (regex',tokens'') -> 
                Just ((Concat regex regex'),tokens'')
            _ -> error "concat missing 2nd operand"
      _ -> error "concat missing 1st operand"
parse (KleeneToken:tokens) = 
    case parse tokens of
      Just (regex,tokens') -> 
          Just ((Repeat regex),tokens')
      _ -> error "Kleene star missing operand"
parse (TermToken c:tokens) = Just (Term c, tokens)
parse (EmptyToken:tokens) = Just (Empty, tokens)


getRegex file = 
    case (parse . tokenize) file of 
      Just (regex,[]) ->
          regex
      _ -> error "regex contains trailing characters"
                  
-- example
readRegex = do
    source <- readFile "regexp1.txt"
    let regex = getRegex source
    putStrLn $ show regex

\end{code}
