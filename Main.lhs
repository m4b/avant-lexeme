\documentclass[11pt]{article}
\usepackage{palatino}
\usepackage{amsmath}

\usepackage[sc]{mathpazo}
\linespread{1.05} % Palatino needs more leading (space between lines)
\usepackage[T1]{fontenc}

\usepackage{graphicx}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{hyperref}

%include polycode.fmt

\title{Compilers Project 1:\\ Scanner Generator\\ }
\author{\textsc{M. Barney, J. Collard, and S. Patel}}
\date{\today}

\begin{document}
\maketitle

\section{Introduction}

This is the final report for project 1, CS454/CS554, on lexical analysis.  It includes all of the source code for our Haskell implementation of a scanner generator.  Our team consists of the authors listed above.  In preparing this document, the work was split equally among the members of the group, in addition to the coding and testing duties.  Approximately 60 man hours went into finishing the project.

Our first design decision was to use Haskell's literate mode to prepare all of our code.  Secondly, we decided to use the distributed revision control software \verb=git= for collaborative coding.

We decided to write each algorithm in the assignment as its own module, in addition to modules describing finite state automata (FSA), regular expressions, and similarly for the file input/output.

Lastly, we decided to write a scanner generator for our final output, instead of a scanner ``interpeter''.  That is, we parse a lexical description, and return a Haskell module to be compiled with an appropriate parser.  Once suitably compiled, the binary scans text files and produces a stream of tokens for the language given in the lexical description.

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

%include ScannerGenerator.lhs

\section{Module: Main.lhs}

The final module, |Main|, puts everything together, by simply calling the function |scannerGenerator| on a lexical description to output a scanner \emph{module} in Haskell source code.

\begin{code}

module Main where

import System.Environment
import ScannerGenerator(scannerGenerator)

main = do
  [contents,moduleName] <- getArgs
  file <- readFile contents
  writeFile (moduleName ++ ".hs") .
   scannerGenerator moduleName $ file

\end{code}

Thus, given a lexical description for a language \(\mathcal{L}\) in a file \verb=<desc.txt>=, a string in file \verb=<string.txt>= (which may or may not be a string in the language \(\mathcal{L}\)), and the binary \verb=<gen>= compiled from our Haskell source code, the command \verb=./gen desc.txt LexDesc= (in a GNU/Linux environment, for example) will produce a scanner module, LexDesc.hs, for \(\mathcal{L}\) which will generate tokens (if possible) from \verb=<string.txt>=, for a suitable parser and |main| function.

Two demo binaries have been provided, \verb=demo1= and \verb=demo2=, which are scanners for the languages given in \verb=lexdesc1.txt= and \verb=lexdesc2.txt=, respectively.

The binaries simply take an ASCII text file as a command line argument, and print the tokens it consumes while reading the string in the file.

The binaries were compiled by simply importing different scanners, i.e., replacing |Scanner1| etc., in the following with the result of running our scanner generator on a lexical description:

%include Demo.lhs

\end{document}