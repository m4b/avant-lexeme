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
\author{\textsc{M. Barney, J. Conrad, and S. Patel}}
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

main = do
  source <- readFile "regexp2.txt"
  let regex = getRegex source
  putStrLn $ show regex

\end{code}

\end{document}