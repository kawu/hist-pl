hist-pl-similar
===============

The hist-pl-similar library can be used for approximate searching in the historical
dictionary of Polish.  It provides a method which will search not only for exact
matches in the dictionary, but also for similar ones.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build `hist-pl-similar`.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install the latest development version of `hist-pl-similar` run:

    cabal install

from the `similar` repository directory.


[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"

<!---
This functionality is provided in a form of a library, but the package provides also
a tool which can be used to search for similar forms of the words supplied in the
standard input.
-->
