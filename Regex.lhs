\section{Regular Expressions}

In this module we give the haskell data type for a regular expression; the encoding almost exactly mirrors the definition given in the assignment.

\begin{code}

{-# LANGUAGE TypeFamilies,
  FlexibleContexts,FlexibleInstances #-}

module Regex(Regex(..)) where

data Regex a = Alt (Regex a) (Regex a) 
             | Concat (Regex a) (Regex a)
             | Kleene (Regex a)
             | Term a
             | Empty deriving (Eq,Show)

\end{code}

We wrote a pretty infix printer:

\begin{verbatim}
instance Show (Regex Char) where
    show (Alt r1 r2   ) = 
        "(" ++ (show r1) ++ "|" ++ (show r2) ++ ")"
    show (Concat r1 r2) = 
        "(" ++ (show r1) ++ "+" ++ (show r2) ++ ")"
    show (Kleene r1)    = "(" ++ (show r1) ++ "*" ++ ")"
    show (Term c)       = '\'':c:[]
    show (Empty)        = "\\epsilon"
\end{verbatim}

However, it was determined that writing a scanner generator would be easier if we used the default show for regular expressions.