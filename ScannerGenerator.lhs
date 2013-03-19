\subsection{Scanner Generator}

This module performs the work necessary to output a scanner, thus making our implementation a scanner \emph{generator}.

CHANGE ALL OF THIS CAUSE IT'S NOT TRUE ANYMORE

From a given lexical description, we first alternate all of the regular expressions found in the classes, then kleene star the entire expression; we then apply Thompson's algorithm, generate a dfa from the nfa (subset construction), and finally apply Hopcroft's minimization algorithm.

OK FROM HERE ON OUT

We then construct (and output to \verb=stdout=) a Haskell program, which when compiled, will accept only those strings given by  the language description that we were originally given.  I.e., the scanner takes a file, \verb=<file>=, as a command line argument, and returns the result of the function |match| (our implementation of algorithm 4) applied to the generated DFA and the strings in \verb=<file>=.


\begin{code}
module ScannerGenerator(scannerGenerator) where
import Regex
import Algorithms
import Input

import System.Environment(getArgs)
import Data.Char(toUpper)

alternate :: [Class] -> Regex Char
alternate [] = Empty
alternate (c:[]) = regex c
alternate (c:cs) =
    Alt (regex c) (alternate cs)

capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = (toUpper s):ss

makeClasses :: [Class] -> String
makeClasses ([]) = 
    "data Token = Token\n" ++
    "     {token :: Kind,\n" ++
    "      location :: Location,\n" ++
    "      relevance :: Relevance}\n"
makeClasses (cl:cls) = 
    let blurb = if cls == [] then "\n" else " |\n" in
    "\t" ++ (capitalize . name $ cl) ++ "Token Char" ++
    blurb ++
    makeClasses cls
    
makeTokenData :: String -> String
makeTokenData s = 
    let tokenData =
            "import ParseLang(Relevance(..))\n\n" ++
            "type Location = Int\n" ++
            "data Kind =\n" ++ 
            (makeClasses . classes . getLang $ s) in
    tokenData

scannerGenerator :: String -> String
scannerGenerator desc = program where
  regex   = Kleene . alternate . classes . getLang $ desc
  dfa     = hopcroft . subsetConstruction . thompson $ regex
  program = "module Main where\n" ++
            "import FiniteStateAutomata(DFA'())\n" ++
            "import Recognize(match)\n" ++
            "import System.Environment(getArgs)\n" ++
            makeTokenData desc ++
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