\begin{code}

module Main where

import Scanner1
import System.Environment

main = do
  [file] <- getArgs
  contents <- readFile file
  let tokens =  scan contents
  putStrLn $ show tokens

\end{code}