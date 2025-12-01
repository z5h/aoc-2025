% -*-Prolog-*-
:- module(d01,
          [a/0, b/0
          ]).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(util).

a :-
    phrase_from_file(dial_zeros_ticks(50-_, Zeros, _), 'input/01/input.txt'),
    format("Zero count is ~10r", [Zeros]).

b :-
    phrase_from_file(dial_zeros_ticks(50-_, Zeros, Ticks), 'input/01/input.txt'),
    Sum is Zeros + Ticks,
    format("Tick count is ~10r", [Sum]).

% read an (L/R) value as a signed int
signed_entry(N) -->
    string(Direction),
    integer(Distance),
    blanks_to_nl, !,
    {(  Direction = `L`
     ->  N is -Distance
     ;   N is Distance
     )}.

% Dial goes from D0-D1,
% with change DD,
% and counts "Zeros" zeroes,
% and "Clicks" clicks
dial_delta_zeros_clicks(D0-D1, DD, Zeros, Clicks) :-
    FullTurns is DD//100,
    Remainder is DD - FullTurns*100,
    D1 is (D0 + Remainder) mod 100,
    (   D1 = 0
    ->  Zeros = 1
    ;   Zeros = 0
    ),
    % rotating clockwise mod 100, the remainder takes us past zero if it decreases
    % the next dial value (to a number > 0).
    % counderclockwise mod 100, the remainder takes us past zero if it increases
    % the next dial value (from a number > 0) 
    (   (Remainder > 0, D0 > D1, D1 > 0)
    ->  ExtraClicks = 1
    ;   (Remainder < 0, D0 < D1, D0 > 0)
    ->  ExtraClicks = 1
    ;   ExtraClicks = 0
    ),
    Clicks is abs(FullTurns) + ExtraClicks.

% parse the input file as signed entries,
% and perform bookkeeping as we go with dial_delta_zeros_clicks/4
dial_zeros_ticks(D0-DN, TotalZeros, TotalClicks) -->
    signed_entry(D),
    {   dial_delta_zeros_clicks(D0-D1, D, Zeros, Clicks)   },
    dial_zeros_ticks(D1-DN, NextZeros, NextClicks),
    {   TotalZeros is Zeros + NextZeros,
        TotalClicks is Clicks + NextClicks
    }.

dial_zeros_ticks(D-D, 0, 0) -->
    blanks_to_nl.
