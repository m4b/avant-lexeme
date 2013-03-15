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

\title{CS454 Project 1:\\« Lexer Analysis »\\}
\author{\textsc{M. Barney, J. Collard, and S. Patel}}
\date{\today}

\begin{document}
\maketitle

\section{Introduction}

This is the final report for project 1, CS454, on lexical analysis.

Our first design decision was to use Haskell's literate mode to prepare all of our code.  Secondly, we decided to use the distributed revision control software \verb=git= for collaborative coding.

We decided to write each algorithm in the assignment as its own module, in addition to modules describing finite state automata (FSA) and regular expressions.

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
%include ParseNFA.lhs
%include ParseDFA.lhs

\section{Module: Main.lhs}

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

From a given lexical description, we first alternate all of the regular expressions found in the classes, then kleene star the entire expression; then we apply Thompson's algorithm, then generate a dfa from the nfa, then apply Hopcroft's minimization algorithm, then finally check whether the dfa recognizes a given strin.

\begin{code}

main = do
  testfile <- readFile "tests/testfile2.txt"
  desc <- getLang "tests/lexdesc2.txt"
--  let (Desc _ _ classes) = desc
--      (greek:integer:floatingPoint:integerArithmatic:punctuation:whitespace:[]) = classes
--      (Class _ regex _) = whitespace
  let regex = Kleene (alternate (classes desc))
  let test = (subsetConstruction . thompson) regex
  putStrLn $ show test 
  putStrLn $ show desc


\end{code}

\end{document}