HistPL
======

HistPL service provides a web interface for the old Polish dictionary.


Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool
to build hist-pl-website.  The easiest way to get both [GHC][ghc] and [Cabal][cabal]
is to install the latest [Haskell Platform][haskell-platform].

Currently, only a development version of the web service is available.
It can be installed directly from this repository:

    cabal install


Usage
=====

The package provides a `hist-pl-website` command-line tool.
It is a standalone server, which can be run in the following way:

    hist-pl-website srpsdp.bin

Where `srpsdp.bin` is a binary version of the dictionary created
with the help of the [hist-pl][hist-pl] tool.
You need to run this command from the current, `website` directory,
since the server uses templates and style sheets located in the
`resources` and `snaplets` directories.
You can set a custom port using the `-p` argument:

    hist-pl-website srpsdp.bin -p 10017

Additional command-line arguments have to be supplied *after* the
dictionary.
Use `hist-pl-website --help` to see the list of all possible server
parameters.


Other dictionaries
------------------

It should be possible to use this service with any other dictionary
representable in terms of the LMF meta-format. 
If you have a dictionary which implements the same subset of LMF
as the old Polish dictionary, you should be able to convert it to
the binary form using the [hist-pl][hist-pl] command-line tool
and run the `hist-pl-website`.


[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[hist-pl]: https://github.com/kawu/hist-pl/tree/master/umbrella#hist-pl "hist-pl"
