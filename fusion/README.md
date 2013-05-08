hist-pl-fusion
==============

The hist-pl-fusion package provides a tool for updating the
[binary version][hist-pl-lexicon] of the historical dictionary
of Polish with contemporary forms present in [PoliMorf][polimorf].

The only element which is actually modified by the fusion is a [DAWG][dawg]
component of the binary dictionary and, therefore, the result of the fusion
is a modified [DAWG][dawg].  The original binary dictionary doesn't change
in the process.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build hist-pl-fusion.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install hist-pl-fusion from the official [Hackage repository][hackage-repo] run:

    cabal install hist-pl-fusion

If you want to upgrade hist-pl-fusion to a newer version you should
update the package list first:

    cabal update 
    cabal install hist-pl-fusion

To install the latest development version from github just run

    cabal install

from the `fusion` repository directory.


Usage
=====

The `hist-pl-fusion` package consists of a `hist-pl-fuse` command line
tool and a library.  The library can be used to query the modified
[DAWG][dawg] component of the dictionary and to perform more general
fusions than the one implemented within the `hist-pl-fuse` tool.

Fusion
------

To use the `hist-pl-fuse` command line tool you need a current version
of [PoliMorf][polimorf] and a binary representation of the historical
dictionary of Polish created with the tool `hist-pl-binarize` tool
(see [hist-pl-lexicon][hist-pl-lexicon] for details).
Then you can run:

    hist-pl-fuse srpsdp.bin PoliMorf-X.tab ana.dawg

where `srpsdp.bin` is a directory of the binary dictionary, `PoliMorf-X.tab`
is a version of [PoliMorf][polimorf] and `ana.dawg` is a resultant, fused
[DAWG][dawg].

Be patient, the process may take several minutes.

The fusion facility has been tested against 0.6.X versions of [PoliMorf][polimorf].

Library
-------

Extensive description of the library interface can be found in the
[NLP.HistPL.Fusion][fusion-module] module of the `hist-pl-fusion`
package.



[hackage-repo]: http://hackage.haskell.org/package/hist-pl-fusion "hist-pl-fusion Hackage repository"
[fusion-module]: http://hackage.haskell.org/packages/archive/hist-pl-fusion/latest/doc/html/NLP-HistPL-Fusion.html "NLP.HistPL.Fusion"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[polimorf]: http://zil.ipipan.waw.pl/PoliMorf "PoliMorf"
[hist-pl-lexicon]: https://github.com/kawu/hist-pl/tree/master/lexicon#hist-pl-lexicon "hist-pl-lexicon package"
[dawg]: http://hackage.haskell.org/package/dawg "Directed acyclic word graph"
