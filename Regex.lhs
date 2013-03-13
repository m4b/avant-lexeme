\section{Regular Expressions}

In this module we give the haskell data type for a regular expression; the encoding almost exactly mirrors the definition given in the assignment.

\begin{code}
module Regex(Regex(..)) where

data Regex a = Alt (Regex a) (Regex a) 
             | Concat (Regex a) (Regex a)
             | Repeat (Regex a)
             | Term a
             | Empty deriving Show
\end{code}