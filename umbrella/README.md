hist-pl
===============

The hist-pl package is a wrapper package covering functionality related to
the historical dictionary of Polish.  It also provides a command-line tool
which can be used to create the binary version of the dictionary and
to perform simple analysis of the input text.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build hist-pl.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install hist-pl from the official [Hackage repository][hackage-repo] run:

    cabal install hist-pl

If you want to upgrade hist-pl to a newer version you should
update the package list first:

    cabal update 
    cabal install hist-pl

To install the latest development version from github just run

    cabal install

from the `umbrella` repository directory.

Morfeusz
--------

Before you use the library, make sure that `Morfeusz` is available.
Otherwise, you will get an error:

    hist-pl: error while loading shared libraries: libmorfeusz.so.0

Morfeusz bindings' library is a dependency of the `hist-pl` package, so
it should be already installed when you try to use the tool.

Usage
=====

The hist-pl package provides a `hist-pl` command-line tool with
the following functionality:

Binarization
------------

To translate the original LMF dictionary into a binary format, use the
`create` mode of the `hist-pl` command-line tool.  Apart from the
LMF dictionary, you have to supply the [PoliMorf][polimorf] dictionary,
which will be used to update the binary dictionary with contemporary
forms.

    hist-pl create srpsdp.xml PoliMorf-X.tab srpsdp.bin

where `PoliMorf-X.tab` is a version of [PoliMorf][polimorf] and
`srpsdp.bin` is a directory to be created for storage of the
binary dictionary.

Since the process involves creating a DAWG version of PoliMorf, it may take
several minutes to complete.

Be aware, that conversion from LMF to the binary format is lossy at the moment.

Printing
--------

To convert the binary dictionary into the LMF format use the `print` mode
of the command line tool:

    hist-pl print srpsdp.bin > srpsdp-prim.xml

Labeling
--------

Use the `analyse` mode to perform a simple dictionary-driven analysis
of the input text.

    hist-pl analyse srpsdp.bin < input.txt

Every line in the input will be treated as a separate sentence.
Then, each sentence will be splited on spaces and punctuation characters.
Finally, the binary dictionary will be searched for every token in the
input data and results of the search will be printed to `stdout`.

Run `hist-pl analyse --help` to learn more about the program arguments and
possible labeling options.

At this moment, the `analyse` mode is provided for presentation purposes.
If you would like to make use of labeling results, you should use the
library API (see next section) and process the results on the application
level, depending on your goals.

Library
-------

The [hist-pl-lexicon][hist-pl-lexicon] library, installed as a dependency
of the `hist-pl` wrapper package, provides a simple interface for accessing
the contents of the binary dictionary.  See the
[NLP.HistPL.Lexicon][hist-pl-module] module for an example of the library
usage and a detailed API description.


[hackage-repo]: http://hackage.haskell.org/package/hist-pl "hist-pl Hackage repository"
[hist-pl-lexicon]: https://github.com/kawu/hist-pl/tree/master/lexicon#hist-pl-lexicon  "hist-pl-lexicon library"
[hist-pl-module]: http://hackage.haskell.org/packages/archive/hist-pl-lexicon/latest/doc/html/NLP-HistPL-Lexicon.html "NLP.HistPL.Lexicon"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[polimorf]: http://zil.ipipan.waw.pl/PoliMorf "PoliMorf"


<!--
Ideas
=====

* Library could provide separate DTD schemas for validation of entire
  dictionary or dictionary fragments.
-->
