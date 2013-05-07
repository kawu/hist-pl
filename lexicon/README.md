polh-lexicon
============

The polh-lexicon library provides a binary representation of the historical dictionary of Polish and language markup format (LMF) parsing utilities which allow to translate the original LMF representation of the dictionary to the binary form. 


Installation
============

To install polh-lexicon from the official [Hackage repository][hackage-repo] run:

    cabal install polh-lexicon

If you want to upgrade polh-lexicon to a newer version you should
update the package list first:

    cabal update 
    cabal install polh-lexicon


Usage
=====

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

TODO


<!--
Ideas
=====

* Library could provide separate DTD schemas for validation of entire
  dictionary or dictionary fragments.
-->
