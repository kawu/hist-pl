hist-pl-poser
=============

The hist-pl-poser tool can be used to assigned POS tags to individual
entries in the historical dictionary of Polish.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build `hist-pl-poser`.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install the latest development version of `hist-pl-poser` run:

    cabal install

from the `poser` repository directory.


Usage
=====

The tool works on the LMF representation of the historical dictionary of Polish.
In order to assign POS tags to individual entries in the `src.xml` LMF
dictionary and write results to the `dst.xml` file, run:

    hist-pl-poser src.xml dst.xml


[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
