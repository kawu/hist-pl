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

### Embeded clusters

    Abba
    Abby
    Abbą
    Abbę
    Abbo
    
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

Where the two last clusters could be joined, probably.
At least, it wouldn't look like a serious error.
