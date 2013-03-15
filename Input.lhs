\section{Input}

Similar to the Algorithms module, in this module we gather all of our solutions for problems 5-8, which deals with file input/output.

Module |ParseFSA| parses either DFAs or FSAs from text file descriptions; |ParseReg| parses regular expressions from text files (with an alphabet) and tests whether symbols occuring in the regular expression are elements of the alphabet provided in the file; and lastly |ParseLang| provides a data structure for lexical descriptions, reads in a complete lexical description from a text file, and transforms it into our internal representation, for use with the algorithms in the |Algorithms| module.

\begin{code}

module Input(
             module ParseFSA,
             module ParseReg,
             module ParseLang) where

import ParseFSA
import ParseReg
import ParseLang


\end{code}