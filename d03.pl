% -*-Prolog-*-
:- module(d03,
          [ a/0,
            b/0
          ]).

% imports
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(util).

test_mode :- false.
input_file(F) :-
    (   test_mode
    ->  F = 'input/03/sample.txt'
    ;   F = 'input/03/input.txt'
    ).

solve(BatteryCount) :-
    input_file(Input),
    phrase_from_file(sequence(digitcount_largest(BatteryCount), Largests), Input),
    sumlist(Largests, Total),
    format("Total is ~10r", [Total]).

a :- solve(2).
b :- solve(12).

digitcount_largest(0, 0) -->
    some_of(digit),
    blanks_to_nl.

digitcount_largest(DC, Total) -->
    {   DC > 0,
        NCD is DC - 1,
        countdown(0'9, 0'1, D)
    },
    some_of(digit),
    digit(D),
    digitcount_largest(NCD, SubTotal),
    { Total is (D - 0'0)*(10^NCD) + SubTotal }.
