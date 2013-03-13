\section{Algorithms}

Our solutions to the ``in-memory'' algorithms given in \S1.2 have been modularized in the following way.

\begin{code}
module Algorithms(module Thompson,
                  module Recognize,
                  module SubsetConstruction) where

import Thompson
import Recognize
import SubsetConstruction

\end{code}

In this way we encapsulated (and named) the solutions individually, as the assignment requested.