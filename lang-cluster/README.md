Language clustering
===================

This program is under construction.
It is meant to perform automatic clustering of a language
(a set of words) into individual lexucal units.


Problem description
===================

The problem is simple: we have a set of words and we want
to cluster those words into groups corresponding to individual
lexemes of the language.

We are going to perform clustering purely on the basis
of edit distance between individual words.

In fact, there are two separate problems which are of
interest to us:
* Perform clustering on the basis of a fixed edit distance function,
* If we describe edit distance as a parametric function, we can
  search for the best set of parameter values with respect to
  a given clustering (e.g. of the contemporary Polish language).

We start with the first problem -- it should lead us to a
reasonable form of the parametric edit distance function.


First steps
===========

Let's start with a rather simple solution:
* Use plain "Levenshtein distance" as a distance metric.
* Use DBSCAN algorithm for clustering.
* Use "dawg" and "adict" libraries, which will provide
  a fast function for retrieving Eps-neighborhood of
  a particular word.


Parameter estimation
--------------------

According to wikipedia, the MinPts parameter (minimum number
of elements in a cluster) should be greater than 1.
The higher value of MinPts, the more dependable clusters
will be (supposedly) found by the clustering algorithm.

For a given metric, the Eps parameter (size of a relevant
neighborhood) can be determined on the basis of an existing
clustering of the contemporary Polish language (or, to be more
precise, the clustering present in the morphological dictionary
of Polish, PoliMorf).  Think about it.


TODO
====

* Currently, the "dawg" and "adict" libraries are separate.
  The "adict" library should be built on top of the "dawg"
  library and use its DAWG representation of an automaton
  dictionary.


Possible extensions
===================

* Use a more general, parametric version of the string metric.
  Remember, that -- due to the usage of DBSCAN clustering method
  -- it has to be a real metric!
