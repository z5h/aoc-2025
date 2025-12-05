% -*-Prolog-*-
:- module(d05,
          [ a_and_b/0
          ]).

% imports
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(util).

test_mode :- false.
input_file(F) :-
    (   test_mode
    ->  F = 'input/05/sample.txt'
    ;   F = 'input/05/input.txt'
    ).

a_and_b :- 
    input_file(Input),
    empty_fdset(E),
    phrase_from_file((ranges(E-FDSet), ranges_fresh_spoiled(FDSet, []-Fresh, []-_Spoiled)), Input),

    length(Fresh, FreshCount),
    format("A: Fresh Count is ~10r", [FreshCount]),
    FreshId in_set FDSet,
    fd_size(FreshId, FreshSize),
    format("B: Fresh ID Size is ~10r", [FreshSize]).

ranges(FDSet0-FDSetN) -->
    integer(A), `-`, integer(B),
    blanks_to_nl,
    {   range_to_fdset(A..B, FDSet),
        fdset_union(FDSet, FDSet0, FDSet1)
    },
    ranges(FDSet1-FDSetN).

ranges(FDS-FDS) -->
    blanks_to_nl. 

ranges_fresh_spoiled(FDS, F0-FN, S0-SN) -->
    integer(Id),
    blanks_to_nl,
    {( Id in_set FDS
    -> F1 = [Id | F0], S1 = S0
    ;  F1 = F0, S1 = [Id | S0]
    )},
    ranges_fresh_spoiled(FDS, F1-FN, S1-SN).

ranges_fresh_spoiled(_, F-F, S-S) -->
    eos.
