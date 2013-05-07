polh-lexicon
============

The polh-lexicon package implements a binary representation of the historical dictionary of Polish.
It also provides language markup format (LMF) parsing utilities which allow to translate the original
LMF representation of the dictionary to a binary form.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build polh-lexicon.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install polh-lexicon from the official [Hackage repository][hackage-repo] run:

    cabal install polh-lexicon

If you want to upgrade polh-lexicon to a newer version you should
update the package list first:

    cabal update 
    cabal install polh-lexicon

To install the latest development version from github just run

    cabal install

from the `polh-lexicon` repository directory.


Usage
=====

The polh-lexicon package consists of tools, which can be used to convert
between the LMF and the binary reprensetation of the dictionary, and a library,
which can be used to programmaticaly communicate with the binary dictionary
(and perform lookup, for example).

Binarization
------------

To binarize the original LMF dictionary into a binary format, use the
`polh-binarize` command line tool.

    polh-binarize srpsdp.xml srpsdp.bin

where `srpsdp.bin` is a directory to be created for a storage of the
binary dictionary.

At this point the conversion to the binary format is lossy.

Printing
--------

To convert the binary dictionary into the LMF format use the `polh-show` command:

    polh-show srpsdp.bin

Library
-------

The library provides a monadic interface to access the contents of the
binary dictionary.  If the binary dictionary resides in a `srpsdp.bin`
directory, you can use the following Haskell code to lookup a word:

```haskell
inc x = x + 1
```


[hackage-repo]: http://hackage.haskell.org/package/polh-lexicon "polh-lexicon Hackage repository"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"


<!--
Ideas
=====

* Library could provide separate DTD schemas for validation of entire
  dictionary or dictionary fragments.
-->
