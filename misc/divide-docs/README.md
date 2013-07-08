divide-docs
===========

Here you can find a couple of tools which can be used to
divide a directory with historical documents into a list
of ID-named works.

The main aspect of this process is that there might be
documents with multiple works in the source directory,
while the resulting directory is flat and there is only
one work per document in it.


Input data
==========

The input data consists of:
* Directory with a hierarchy of historical documents,
* CSV file with ID, Title and Path-of-Document column types.

It is assumed that paths from the CSV file correspond to
documents from the directory hierarchy.
It is also assumed, that the CSV paths are kept using the
Windows convention, while the script is run on the Linux
machine.


move-simple
===========

The `move-simple` script can be used to move and rename all
documents to which only one work (according to the CSV file)
is assigned in the directory hierarchy.
You can run it using the following command:

    runhaskell move_simple.hs source-dir meta.csv dest-dir

where `source-dir` is a source directory with historical documents,
`meta.csv` is a CSV file, and `dest-dir` is a destination directory
to which the documents will be copied.


move-compound
=============

The `move-compound` script can be used to move and rename all
documents in which more than one work is stored.

    runhaskell move_compound.hs source-dir meta.csv source-sub-path dest-dir

where `source-sub-path` is a path (written using the Windows
convention) to a particular compound document in the `source-dir`
directory.  Use the `move-simple` command to see which documents
are compound.


rm-prefs
========

The list of documents created by the `move-compound` command
are prefixed with a "cutting-point" position.
The reason behind this behaviour is that we want to be able
to browse through the set of created files in order consistent
with the original, compund document.

To remove prefixed from individual files, use the `rm-prefs`
command:

    runhaskell rm_prefs.hs dest-dir-prefs dest-dir

where `dest-dir-prefs` is a directory with a list of documents
obtained using the `move-compund` command and `dest-dir` is a
directory where the resulting documents -- with prefixes removed
-- will be put.


check-consistent
================

To check consistensy between the CSV file and the resulting
directory of ID-named documents, use the following command:

    runhaskell check_consistent.hs divided meta.csv

where `meta.csv` is the CSV file and `divided` is a directory
obtained by using the tools desribed above.
