\subsection{Scanner Generator}

This module performs the work necessary to output a scanner, thus making our implementation a scanner \emph{generator}.

\begin{code}
module ScannerGenerator(scannerGenerator) where
import Regex
import Algorithms
import Input

alternate :: [Class] -> Regex Char
alternate [] = Empty
alternate (c:[]) = regex c
alternate (c:cs) =
    Alt (regex c) (alternate cs)

scannerGenerator :: String -> String
scannerGenerator desc = program where
  regex   = Kleene . alternate . classes . getLang $ desc
  dfa     = hopcroft . subsetConstruction . thompson $ regex
  program = "module Main where\n" ++
            "import FiniteStateAutomata(DFA'())\n" ++
            "import Recognize(match)\n" ++
            "dfa :: DFA' Char\n" ++ 
            "dfa = read \"" ++ 
            (replace '"' "\\\"") (show dfa) ++ "\"\n" ++
            "main = do { contents <- getContents;" ++
            " putStrLn $ (show (match dfa contents)) }"

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace a b = concatMap replace' where
  replace' x = if a == x then b else [x]

\end{code}