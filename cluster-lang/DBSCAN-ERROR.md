Error analysis for the DBSCAN algorithm
=======================================

Here we analyse some typical errors obtained with the following
set of parameters:

    cluster-lang -m 3 -b 0.1 -e 1.0 -d posmod < ...

on a small sample of PoliMorf.


Example 1
=========

### What we got:

    Abba
    Abbas
    Abbasa
    Abbasach
    Abbasami
    Abbasem
    Abbasie
    Abbasom
    Abbasowi
    Abbasowie
    Abbasy
    Abbasyda
    Abbasydach
    Abbasydami
    Abbasydo
    Abbasydom
    Abbasydy
    Abbasydzi
    Abbasydzie
    Abbasydów
    Abbasydą
    Abbasydę
    Abbasów
    Abbo
    Abbot
    Abbota
    Abbotach
    Abbotami
    Abbotem
    Abbotom
    Abbotowi
    Abbotowie
    Abbott
    Abbotta
    Abbottach
    Abbottami
    Abbottem
    Abbottom
    Abbottowi
    Abbottowie
    Abbotty
    Abbottów
    Abboty
    Abbotów
    Abby
    Abbą
    Abbę

### Embeded clusters:


#### Abba

    Abba
    Abby
    Abbą
    Abbę
    Abbo

#### Abbas
    
    Abbas
    Abbasa
    Abbasach
    Abbasami
    Abbasem
    Abbasie
    Abbasom
    Abbasowi
    Abbasowie
    Abbasy
    Abbasów

#### Abbasyda

    Abbasyda
    Abbasydach
    Abbasydami
    Abbasydo
    Abbasydom
    Abbasydy
    Abbasydzi
    Abbasydzie
    Abbasydów
    Abbasydą
    Abbasydę

#### Abbot
    
    Abbot
    Abbota
    Abbotach
    Abbotami
    Abbotem
    Abbotom
    Abbotowi
    Abbotowie
    Abboty
    Abbotów

#### Abbott
    
    Abbott
    Abbotta
    Abbottach
    Abbottami
    Abbottem
    Abbottom
    Abbottowi
    Abbottowie
    Abbotty
    Abbottów

Analysis
--------

It can be easily seen that individual clusters are internally very "dense".
They are also similar to each other, but not so much.

Let's analyse the 'Abba' cluster.  The following list presents neighbours
of distance 1 for each element of the cluster:

    Abba =>      Abby Abbą Abbę Abbo
    Abby => Abba      Abbą Abbę Abbo
    Abbą => Abba Abby      Abbę Abbo
    Abbę => Abba Abby Abbą      Abbo
    Abbo => Abba Abby Abbą Abbę

It's a clique with respect to the levenshtein distance function!
But, w.r.t. the 'Abbas' cluster, there is only one 1-distance relation:

    Abba => Abbas

While all the other 'Abba' elements are not connected to any other
'Abbas' elements.  And yet, both clusters are joined in the clustering
result.


Summary
=======

To sum up: the problem with DBSCAN is that it allows different elements of
the same cluster to be very far from each other, as long as there is
a connecting path between them.  That is an undesirable feature of this
clustering method.
