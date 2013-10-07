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


Usage
=====

The library provides a function for approximate searching which can be used
from the level of [GHCi][ghci] session or from your own library.
Here is an example of a [GHCi][ghci] session in which the
[hist-pl-lexicon][hist-pl-lexicon] library is used to open the binary version
of the historical dictionary.

```haskell
> :set -XOverloadedStrings
> import qualified NLP.HistPL.Lexicon as H
> import qualified NLP.HistPL.Similar as H
>
> hpl <- H.open "srpsdp.bin"
> H.lookupSim hpl "abdankowała" 0.5
Just ("abdankowali",0.3333333333333333)
> H.lookupSim hpl "abdankowaliśmy" 0.5
Nothing
> H.lookupSim hpl "abdankowaliśmy" 1
Just ("abdankowali",0.75)
```

The first argument of the `lookupSim` function is an opened dictionary,
the second one is a particular word we are looking for, and the last
argument is a maximum edit distance between the searched word and the
resulting match.


[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html "GHCi"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[hist-pl-lexicon]: https://github.com/kawu/hist-pl/tree/master/lexicon#hist-pl-lexicon  "hist-pl-lexicon library"

<!---
This functionality is provided in a form of a library, but the package provides also
a tool which can be used to search for similar forms of the words supplied in the
standard input.
-->
