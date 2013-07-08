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

