% -*-Prolog-*-
:- module(d02,
          [ a/0,
            b/0
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
    ->  F = 'input/02/sample.txt'
    ;   F = 'input/02/input.txt'
    ).

a :-
    input_file(Input),
    phrase_from_file(ranges(Ranges), Input),
    maplist(pattern_range_value(duplicated), Ranges, Values),
    sumlist(Values, Total),
    format("Total is ~10r", [Total]).

b :-
    input_file(Input),
    phrase_from_file(ranges(Ranges), Input),
    maplist(pattern_range_value(repeated), Ranges, Values),
    sumlist(Values, Total),
    format("Total is ~10r", [Total]).

% Pattern is for the digits pattern. It can be duplicated or repeated.
% Range is the input range
% Value is the result of summing matching numbers 
pattern_range_value(Pattern, From-To, Value) :-
    findall(Int, range_pattern_id(From-To, Pattern, Int), Ints),
    sort(Ints, Unique),
    sumlist(Unique, Value).

% parse ranges
ranges(Ranges) -->
    sequence(range, `,`, Ranges),
    blanks,
    eos.

% parse a range
range(From-To) -->
    integer(From),
    `-`,
    integer(To).

% true when S is a sequence of a duplicated subsequence
duplicated(S) :-
    append(A, A, S).

all_same([_]).
all_same([H,H|T]) :- all_same([H|T]).

% true when R is a list of repeating sublists
repeated(R) :-
    length(R, Max),
    between(2, Max, RepeatsLength),
    length(Repeats, RepeatsLength),
    all_same(Repeats),
    append(Repeats, R).
    
% true when we have a list of digits without a leading 0 (except for [0])
digits([0]).
digits([H|T]) :-
    H in 1..9,
    digits_tail(T).

digits_tail([]).
digits_tail([H|T]) :-
    H in 0..9,
    digits_tail(T).

% true when value V is represented by the list of digits D    
digits_value(D, V) :-
    digits(D),
    reverse(D, R),
    reversed_digits_value(R, V).

reversed_digits_value([], 0).
reversed_digits_value([D | Rest], V) :-
    reversed_digits_value(Rest, V0),
    V #= D + 10 * V0.

% true when L2 is the same length as L1, or is longer
same_or_larger_length(L1, L2) :-
    (   same_length(L1, L2)
    ;   L2 = [_|Rest],
        same_or_larger_length(L1, Rest)
    ).

% D are digits representing a number >= N
number__larger_digits(N, D) :-
    number_codes(N, C),
    same_or_larger_length(C, D),
    V #>= N,
    digits_value(D, V).

% D are digits representing a number =< N
number__smaller_digits(N, D) :-
    number_codes(N, C),
    same_or_larger_length(D, C),
    V #=< N,
    digits_value(D, V).        

:- meta_predicate(range_pattern_id(?, 1, ?)).
% Pattern is a pattern we want the digits to follow
% From-To is our range
% Id is a number in the range that patches the pattern
range_pattern_id(From-To, Pattern, Id) :-
    number__smaller_digits(To, D),
    number__larger_digits(From, D),
    call(Pattern, D),
    digits_value(D, Id),
    label(D).
