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
  The process should be language-agnostic.
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
* Weight of substitution between lower/upper version
  of the same character.
* Weight of substitution between similar characters.

The two last propositions (especially the last) seem
a little controversial.  It's hard to determine, which
two characters should be treated as similar.
Perhaps it would be better to disregard this proposition
in our first experiments.


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


Comparing equivalence relations
-------------------------------

### Note

We would like the evaluation function to satisfy
the following property: it is a serious mistake to
join multiple lexemes into one lexeme, much more serious
than getting several lexemes in place of one lexeme.  
Motivation: if we were to manaully amend clustering
results, it would be much simpler two join two clusters
(just a matter of selecting them) than to divide an existing
one.

Back to our task: how do we compare relations, two
subsets of `WxW`?  Let's assume, that we will be doing
computations on a disjoint-set forest data structure.

### Warm-up question

How do we enumerate all relation elements in a disjoint-set forest?

We can do that by going through all `W` elements and -- for
each element `v in W` -- enumerating all pairs `(v, w)` for
each ancestor of `v`.  But, unfortunately, we cannot do that
easily, because the disjoint-set forest is flattened and a
parent of `v` always points two the equivalence class representant!
Therefore, we would get only relations between elements and their
class representants this way.

A solution to this problem would be to not to flatten the
disjoint-set forest at all.  It would be a little less
efficiet, but probably imperceptibly so, given that
most of the time is most likely spent in approximate
dictionary searching.

###  Difference between disjoint-set forests

In order to compute difference between two sets we find
all elements in the first set which are not in the second
one (trivial).  We can enumerate all relation pairs from the
first disjoint-set forest, we just need to be able to determine
if a relation pair is an element of the second disjoint-set
forest.

### Membership

To determine membership of a `(v, w)` pair in a disjoint-set
forest we just check if both `v` and `w` elements belong to
the same equivalence class (i.e. have the same representant).

### Caveat

Computational complexity of the method described above is
proportional to the size of the equivalence relations which
can be quadratic with respect to the size of `W`.


Comparing equivalence relations v2
----------------------------------

Perhaps we can come up with another measure of similarity
between relations.  A measure which can be computed in
a quicker way.

### Proposition

We can try to compute the number of clusters in a reference
relation corresponding to each cluster in the computed relation.
Than we sum up the obtained numbers and get the final result.

To find clusters in a reference relation corresponding to
a given cluster, we:
* Enumerate over all elements of the cluster and, for each element,
* Determine cluster of this element in the second relation.

Simple!  We only have to be able to enumerate over all elements
of the chosen cluster in a row.  For that, we may need a different
representation of a cluster.

### Direction 

The process above can be performed in two directions -- using the
reference clustering as the second relation, or using the computed
clustering as the second relation.  To compute the final similarity
measure value, we should perform both computations, but -- keeping
in mind, that it is more important to not to construct big clusters
consisting of several reference clusters -- it might be a good idea
to assign different weights to both versions.


Data representation of an equalivalence relation
------------------------------------------------

From the "typological" point of view, it's just a
    
    type EqRel = Set (Set Word),

where all sets are mutually disjoint. In a file we can
represent a relation as an empty-line-separated sections
containing subsequent clusters.

### Application-level representation

On the level of application we will need a more efficient
representation of an equivalence relation.
It will also need to provide the folowing operations: 
* Enumerate over relation (gives a list of clusters),
* Enumerate over a chosen cluster,
* Find relation corresponding to a chosen word (if present).

First of all, we can encode all words using a DAWG. 
Than, since we know the number of words `N`, and to each
word a unique number from the `{0..N-1}` domain is assigned,
we can keep info about the clustering in an array of size `N`.



Misc
====

Parameters, which seemed to be acceptable:

    cluster-lang -m 3 -b 0.1 -e 1 -d posmod
