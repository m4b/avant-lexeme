\begin{code}
module Regex(Regex(..)) where

data Regex a = Alt (Regex a) (Regex a) 
             | Concat (Regex a) (Regex a)
             | Repeat (Regex a)
             | Term a
             | Empty
\end{code} 