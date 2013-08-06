cluster-lang
===============

The cluster-lang package implements unsupervised lexeme-clustering
algorithm based on DBSCAN and edit distance.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build cluster-lang.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

To install the latest development version from github run

    cabal install

from the `cluster-lang` repository directory.


Usage
=====

The cluster-lang package provides a `cluster-lang` command-line tool which
can be used to cluster a list of words specified in the input file.

Input
-----

The input data should consists of a list of words, each word in a separate line.
Words don't have to be given in any specific order and duplicates are acceptable.
For example:

    psalm
    jako
    rzecz
    piękna
    jako
    rzecz
    przyjemna
    patrząc
    gdzie
    miłość
    panuje
    wzajemna
    ...


Output
------

The program will output the set of generated lexemes, each lexeme presented
in a separate line as a space-separated list of forms.


Clustering
----------

To cluster the `input.txt` file use the following command:

    cluster-lang < input.txt

The program takes several optional parameters which control the clustering
process.  Some of the more useful optios are:
* `--minpts`: Minimum number of elements required to form a cluster.
* `--baseeps` and `--epsmax`: DBSCAN `epsilon` parameter for a given
  word is computed as `min(baseeps*n, epsmax)` where `n` is the lenght
  of the word.
* `--dist`: Type of the edit distance function.  Possible choices are:
  `levenshtein` (default) and `posmod` (position modifier is used
  to decrease costs of edit operations performed closer to the end
  of the word).

Run `cluster-lang --help` to see the complete list of program arguments.


[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
