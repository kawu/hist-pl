Error analysis
==============

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

Let's analyse the 'Abba' cluster:
    Abba =>      Abby Abbą Abbę Abbo
    Abby => Abba      Abbą Abbę Abbo
    Abbą => Abba Abby      Abbę Abbo
    Abbę => Abba Abby Abbą      Abbo
    Abbo => Abba Abby Abbą Abbę

It's a clique with respect to the levenshtein distance function!
Unfortunately, our clustering method doesn't take this on acount
in any way...
