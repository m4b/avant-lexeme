\subsection{Scanner Generator}

This module performs the work necessary to output a scanner, thus making our implementation a scanner \emph{generator}.

From a given lexical description, we first alternate all of the regular expressions found in the classes, then kleene star the entire expression; then we apply Thompson's algorithm, then generate a dfa from the nfa, then apply Hopcroft's minimization algorithm, then finally check whether the dfa recognizes a given string.

We then construct (and output to \verb=stdout=) a Haskell program, which when compiled, will accept only those strings given by  the language description that we were originally given.


\begin{code}
module ScannerGenerator(scannerGenerator) where
import Regex
import Algorithms
import Input
import System.Environment(getArgs)

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
            "import System.Environment(getArgs)\n" ++
            "dfa :: DFA' Char\n" ++ 
            "dfa = read \"" ++ 
            (replace '"' "\\\"") (show dfa) ++ "\"\n" ++
            "main = do \n \t args <- getArgs\n" ++
            "\t let [contents] = args\n" ++  
            "\t string <- readFile contents\n" ++
            "\t putStrLn $ (show (match dfa string))\n"

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace a b = concatMap replace' where
  replace' x = if a == x then b else [x]

\end{code}