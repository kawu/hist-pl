hist-pl-collect
===============

The hist-pl-collect package implements functionality intended for
constructing LMF dictionaries from texts.


Usage
=====

To acquire the list of words (each word presented in a separate line) kept in a
directory of text documents, run the following command:

    hist-pl-collect collect path path-to-directory > words.txt

After collecting words you can also lower-case those which occur usually in
the lower-cased form.

    hist-pl-collect lower words.txt > lowered.txt
