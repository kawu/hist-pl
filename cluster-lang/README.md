Language clustering
===================

This program is under construction.
It is meant to perform automatic clustering of a language
(a set of words) into individual lexical units.


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


DONE
====

* Currently, the "dawg" and "adict" libraries are separate.
  The "adict" library should be built on top of the "dawg"
  library and use its DAWG representation of an automaton
  dictionary.
* Package the language clustering funcionality into a
  separate Haskell package.


TODO
====

* Estimate custom cost parameters on the basis of PoliMorf.
  The process should be dictionary-agnostic.
* Translate raw text into a list of words.  It should be kept
  in mind, that there are some strage artifacts in historical
  texts.
  For example, `<`, `>` and `{`, `}` characters are used to
  represent some uncertain (on the level of OCR) cases (e.g.
  "spu<ś>ci").
  We can either translate such cases to normal text form
  or just ignore them. 
* Adding contexts to individual dictionary entries on the
  basis of a textual data.
* Merging two dictionaries.  We have an LMF dictionary
  constructed on the basis of the Reczek's dictionary
  and we can merge it with the automatically acquired
  one.  This has a low priority and would be of rather
  theoretical value right now, though.


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

Parameters
----------

We should consider the following set of parameters,
presented in order of decreasing importance:
* MinPts DBSCAN parameter.
* BaseEps and epsMax parameters.
* Position modifier, contolled with two parameters.
  The first one determines, from which position the
  modifier starts to tilt down.  The second one --
  to what value the modifier tilts at the end of the
  word.  Perhaps we should replace these two parameters
  with only one, at first, by assuming that the modifier
  starts to tilt down from the first position.
* Weight of substitution between similar characters.
* Weight of substitution between lower/upper version
  of the same character.


Evaluation
----------

In order to determine optimal parameter values, we need to
be able to perform evaluation on an existing clustering.
In our case we will use PoliMorf.

Our goal is to compare a clustering with a reference clustering 
and determine how similar both clusterings are.  The greater
similarity between both clusterings, the better evaluation
result should be obtained.

Mathematically, clustering is an equivalence relation defined
w.r.t the set of words `W`.  So we have two relations (subsets
of `WxW`) and, in order to compare them, we can e.g. compute
the size of symetric difference between them.


Possible extensions
===================

* Use a more general, parametric version of the string metric.
  Remember, that -- due to the usage of DBSCAN clustering method
  -- it has to be a real metric!
