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

We start with the first problem, because it should lead us to a
reasonable form of the parametric edit distance function.
