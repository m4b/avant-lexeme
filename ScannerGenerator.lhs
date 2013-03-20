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

import Data.List(intersperse)

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

writeImports :: String -> String
writeImports moduleName = "module " ++ moduleName ++ "(scan) where \n" ++ 
                          concat ["import FiniteStateAutomata(DFA'(..))\n",
                                  "import ParseLang(Relevance(..))\n",
                                  "import Recognize(tokenize)\n",
                                  "import Data.List(maximumBy,lookup)\n",
                                  "import Data.Maybe(fromJust)\n",
                                  "type Location = Int\n"]

writeClasses :: [Class] -> String
writeClasses classes = "data Class = " ++ names ++ " deriving Show\n" where
  names = concat . intersperse " | " . map (capitalize . name) $ classes

writeTokens :: String
writeTokens = "data Token = Token {kind :: Class, relevance :: Relevance, location :: Location, token :: String} deriving Show\n"

writeDFA :: Class -> String
writeDFA c = name' ++ " :: DFA' Char\n" ++
             name' ++ " = read \"" ++ dfa ++ "\"\n" where
  name' = name c
  dfa   = replace '"' "\\\"" . show . hopcroft . subsetConstruction . thompson . regex $ c
  
writeDFAs :: [Class] -> String
writeDFAs cs = "dfas :: [DFA' Char]\n" ++
               "dfas = " ++ names ++ "\n" where
  names = replace '"' "" . show . map name $ cs

writeConstructor :: Class -> String
writeConstructor c = name' ++ " :: Location -> String -> Token\n" ++
                     name' ++ " = Token " ++ capName ++ " " ++ rel ++ "\n" where
  name' = name c ++ "'"     
  capName = capitalize . name $ c
  rel = show . relevance $ c

writeConstructors :: [Class] -> String
writeConstructors cs = "constructors :: [Location -> String -> Token]\n" ++
                       "constructors = " ++ names ++ "\n" where
  names = replace '"' "" . show . map ( (++ "'") . name) $ cs

writeScan = concat ["table :: [(DFA' Char, Location -> String -> Token)]\n",
                    "table = zip dfas constructors\n",
                    "getToken :: DFA' Char -> Location -> [Char] -> Token\n",
                    "getToken dfa loc token = construct loc token where\n",
                    "  construct = fromJust . lookup dfa $ table\n",
                    "scan :: String -> [Token]\n",
                    "scan = scan' [] where\n",
                    "  scan' :: [Token] -> String -> [Token]\n",
                    "  scan' acc s = if done then reverse (t:acc) else reCURSE where\n",
                    "    reCURSE = if err then error \"Could not parse.\" else scan' (t:acc) rem\n",
                    "    done = rem == []\n",
                    "    err = consumed == 0\n",
                    "    list = zip dfas . map (tokenize s) $ dfas\n",
                    "    (dfa,(consumed, token, rem)) = maximumBy compareTrip list\n",
                    "    t = getToken dfa consumed token\n",
                    "    compareTrip (_,(i,_,_)) (_,(i2,_,_)) = compare i i2"]

scannerGenerator :: String -> String -> String
scannerGenerator moduleName desc = program where
  (Desc _ _ classes) = getLang desc
  dfas = concatMap writeDFA classes
  constructors = concatMap writeConstructor classes
  program = writeImports moduleName ++ writeClasses classes ++ writeTokens ++ dfas ++ writeDFAs classes ++ constructors ++ writeConstructors classes ++ writeScan



replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace a b = concatMap replace' where
  replace' x = if a == x then b else [x]

\end{code}