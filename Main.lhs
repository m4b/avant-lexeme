\documentclass[11pt]{article}
\usepackage{palatino}
\usepackage{amsmath}
\usepackage{fontspec}
%\setmainfont[Ligatures=TeX]{Linux Libertine O}
\newfontfamily\fallbackfont{DejaVu Sans}

\usepackage{newunicodechar}
%\newunicodechar{☣}{{\fallbackfont ☣}}
\newunicodechar{«}{{\fallbackfont «}}
\newunicodechar{»}{{\fallbackfont »}}
%\newunicodechar{ε}{{\fallbackfont ε}}

\usepackage[sc]{mathpazo}
\linespread{1.05} % Palatino needs more leading (space between lines)
% \usepackage[T1]{fontenc}

\usepackage{graphicx}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{hyperref}

%include polycode.fmt

\title{CS454 Project 1:\\« Lexer Generator »\\}
\author{\textsc{M. Barney, J. Collard, and S. Patel}}
\date{\today}

\begin{document}
\maketitle

\section{Introduction}

This is the final report for project 1, CS454, on lexical analysis.

Our first design decision was to use Haskell's literate mode to prepare all of our code.  Secondly, we decided to use the distributed revision control software \verb=git= for collaborative coding.

We decided to write each algorithm in the assignment as its own module, in addition to modules describing finite state automata (FSA) and regular expressions.

Lastly, we decided to write a lexer generator for our final output.  That is, we parse a lexical description, and return a Haskell source file to be compiled.  Once suitably compiled, the binary will accept text files which are accepted if and only if they are strings accepted by the language given in the lexical description.

%include FiniteStateAutomata.lhs
%include Regex.lhs

%include Algorithms.lhs
%include Thompson.lhs
%include SubsetConstruction.lhs
%include Hopcroft.lhs
%include Recognize.lhs

%include Alphabet.lhs
%include Input.lhs
%include ParseReg.lhs
%include ParseFSA.lhs
%include ParseLang.lhs

\section{Module: Main.lhs}

The final module, |Main|, puts everything together.

\begin{code}

module Main where

import FiniteStateAutomata
import Regex
import Algorithms
import Input

alternate (c:[]) = regex c
alternate (c:cs) =
    Alt (regex c) (alternate cs)

\end{code}

From a given lexical description, we first alternate all of the regular expressions found in the classes, then kleene star the entire expression; then we apply Thompson's algorithm, then generate a dfa from the nfa, then apply Hopcroft's minimization algorithm, then finally check whether the dfa recognizes a given string.

\begin{code}

main = do
  testfile <- readFile "tests/testfile3.txt"
  desc <- getLang "tests/lexdesc3.txt"
  let regex = Kleene (alternate (classes desc))
  let test = ((match . hopcroft . subsetConstruction . thompson) regex) testfile
  putStrLn $ show test 
  putStrLn $ show desc


\end{code}

\end{document}