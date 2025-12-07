% -*-Prolog-*-
:- module(d07,
          [ a/0
          , b/0
          ]).

% imports
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(util).

test_mode :- false.
input_file(F) :-
    (   test_mode
    ->  F = 'input/07/sample.txt'
    ;   F = 'input/07/input.txt'
    ).

a :-
    input_file(Input),
    phrase_from_file((some_of(binary_number, Ns), eos), Input),
    exclude('='(0), Ns, [Beam | SplittersList]),
    in__splitters_list__out__total(Beam, SplittersList, _, Total),
    writeln(total:Total).

b :-
    input_file(Input),
    phrase_from_file((some_of(binary_number, Ns), eos), Input),
    exclude('='(0), Ns, [Beam | SplittersList]),
    in__quantum_splitters_list__worlds([Beam-1], SplittersList, Total),
    writeln(total:Total).

bit(0) --> `.`.
bit(1) --> `S`.
bit(1) --> `^`.
 
binary_number(N) -->
    bit(B),
    binary_number(M),
    {   N is M*2 + B }.

binary_number(0) -->
    blanks_to_nl.

beams_splitters_beams_count(Beams, Splitters, NewBeams, Count) :-
    SplitAt is Beams /\ Splitters,
    Count is popcount(SplitAt),
    Passed is Beams xor SplitAt,
    NewBeams is (SplitAt << 1) \/ (SplitAt >> 1) \/ Passed.

in__splitters_list__out__total(In, [], In, 0).
in__splitters_list__out__total(In, [Splitters | Rest], Out, Total) :-
    beams_splitters_beams_count(In, Splitters, NextIn, Count),
    in__splitters_list__out__total(NextIn, Rest, Out, CountRest),
    Total is Count + CountRest.    

% a quantum beam is B-C B is the set bit, and C is the number of ways it arrived
quantumsplitters__beam__out(S, B-C, Out) :-
    At is B /\ S,
    (   At = 0
    ->  Out = [B-C]
    ;   B1 is At >> 1, B2 is At << 1, Out = [B1-C, B2-C] 
    ).

key_list__key_sum(K-L, K-S) :- sumlist(L, S).

in__quantum_splitters_list__out(BCs, [], BCs).
in__quantum_splitters_list__out(BCs, [QSH|QST], O) :-
    maplist(quantumsplitters__beam__out(QSH), BCs, ListOfOuts),
    flatten(ListOfOuts, NextBeamCounts),
    sort(1, @>=, NextBeamCounts, Sorted),
    group_pairs_by_key(Sorted, BeamCountList),
    maplist(key_list__key_sum, BeamCountList, NextBCs),
    in__quantum_splitters_list__out(NextBCs, QST, O).

in__quantum_splitters_list__worlds(I, S, C) :-
    in__quantum_splitters_list__out(I, S, O),
    pairs_values(O, Values),
    sumlist(Values, C).
    
