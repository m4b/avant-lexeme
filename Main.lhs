\documentclass[11pt]{article}
%\usepackage{palatino}
%\usepackage{amsmath}
\usepackage{fontspec}
\setmainfont[Ligatures=TeX]{Linux Libertine O}
\newfontfamily\fallbackfont{DejaVu Sans}

\usepackage{newunicodechar}
%\newunicodechar{☣}{{\fallbackfont ☣}}
\newunicodechar{«}{{\fallbackfont «}}
\newunicodechar{»}{{\fallbackfont »}}
%\newunicodechar{ε}{{\fallbackfont ε}}

% \usepackage[sc]{mathpazo}
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

This is the final report for project 1, CS454, for lexical analysis.

Our first design decision was to use Haskell's literate mode to prepare all of our code.  Secondly, we decided to use the distributed revision control software \verb=git= for collaborative coding.

%include FiniteStateAutomata.lhs
%include Regex.lhs
% include Algorithms.lhs

\section{Module: Main.lhs}

\begin{code}

module Main where

import FiniteStateAutomata
import Regex
--import Algorithms


main =
     putStrLn "((λ.x x) helloworld)"

\end{code}

\end{document}