hist-pl-lexicon
===============

The hist-pl-lexicon package implements a binary representation of the historical dictionary of Polish.
It also provides language markup format (LMF) parsing utilities which allow to translate the original
LMF representation of the dictionary to a binary form.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build hist-pl-lexicon.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install hist-pl-lexicon from the official [Hackage repository][hackage-repo] run:

    cabal install hist-pl-lexicon

If you want to upgrade hist-pl-lexicon to a newer version you should
update the package list first:

    cabal update 
    cabal install hist-pl-lexicon

To install the latest development version from github just run

    cabal install

from the `hist-pl-lexicon` repository directory.


Usage
=====

The hist-pl-lexicon package consists of tools, which can be used to convert
between the LMF and the binary reprensetation of the dictionary, and a library,
which can be used to communicate with the binary dictionary from a level
of application (to perform lookup, for example).

Binarization
------------

To translate the original LMF dictionary into a binary format, use the
`hist-pl-binarize` command line tool.

    hist-pl-binarize srpsdp.xml srpsdp.bin

where `srpsdp.bin` is a directory to be created for a storage of the
binary dictionary.

Be aware, that conversion from LMF to the binary format is lossy at the moment.

Printing
--------

To convert the binary dictionary into the LMF format use the `hist-pl-show`
command line tool:

    hist-pl-show srpsdp.bin > srpsdp-prim.xml

Library
-------

The library provides a simple interface for accessing the contents of
the binary dictionary.  See the [NLP.HistPL][hist-pl-module] module
for an example of the library usage and a detailed API description.


[hackage-repo]: http://hackage.haskell.org/package/hist-pl-lexicon "hist-pl-lexicon Hackage repository"
[hist-pl-module]: http://hackage.haskell.org/packages/archive/hist-pl-lexicon/latest/doc/html/NLP-HistPL.html "NLP.HistPL"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"


<!--
Ideas
=====

* Library could provide separate DTD schemas for validation of entire
  dictionary or dictionary fragments.
-->
