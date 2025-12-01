% -*-Prolog-*-
:- module(d00,
          [a/0
          ]).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(util).

a :-
	ThisFile = 'template.pl',
    phrase_from_file(any(A), ThisFile),
    list_length(A, Result),
    format("Length, in binary, is ~2r", [Result]).
