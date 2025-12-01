% -*-Prolog-*-
:- module(util,
          [
          	list_length/2,
          	seq//1,
            seqq//1,
            ignore//0,
            any//1
          ]).

list_length(Li, Le) :- length(Li, Le).

% DCG helpers taken/inspired trom https://www.metalevel.at/prolog/dcg 

% A sequence
seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

% a sequence of sequences
seqq([]) --> [].
seqq([Es|Ess]) --> seq(Es), seqq(Ess).

% ignore
ignore --> [].
ignore --> [_], ignore.

any([]) --> [].
any([H|T]) --> [H], any(T).
