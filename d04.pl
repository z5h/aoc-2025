% -*-Prolog-*-
:- module(d04,
          [ a/0,
            b/0
          ]).

% imports
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(rbtrees)).
:- use_module(util).


test_mode :- false.
input_file(F) :-
    (   test_mode
    ->  F = 'input/04/sample.txt'
    ;   F = 'input/04/input.txt'
    ).

a :- 
    input_file(Input),
    rb_empty(E),
    phrase_from_file(rolls_data(E-Rolls, 0, 0), Input),
    rolls_accessible(Rolls, A),
    length(A, Result),
    writeln(Result).

b :- 
    input_file(Input),
    rb_empty(E),
    phrase_from_file(rolls_data(E-Rolls, 0, 0), Input),
    rolls_count(Rolls, InitialCount),
    rolls_cleared(Rolls, Remaining),
    rolls_count(Remaining, RemainingCount),
    Result is InitialCount - RemainingCount,
    writeln(Result).

is_roll('@').

at_neighbour(C-R, CN-RN) :-
    member(DC, [-1, 0, 1]),
    member(DR, [-1, 0, 1]),
    CN is C + DC,
    RN is R + DR,
    C-R \= CN-RN.

rb_accumulate(Pred, Key-Value, A0, A1) :-
    (   call(Pred, Key-Value)
    ->  A1 = [Key | A0]
    ;   A1 = A0
    ).
    

rolls_entry_accessible(Rolls, At-Value) :-
    is_roll(Value),
    findall(
        '@',
        (   at_neighbour(At, N),
            rb_lookup(N, '@', Rolls)
        ),
        Blockers
        ),
    length(Blockers, L),
    L < 4.        

rolls_accessible(R, A) :-
    rb_fold(rb_accumulate(rolls_entry_accessible(R)), R, [], A).

rb_delete_all(T, [], T).
rb_delete_all(T, [K|Ks], TN) :-
    rb_delete(T, K, T1),
    rb_delete_all(T1, Ks, TN).

rolls_cleared(R, C) :-
    rolls_accessible(R, A),
    (   A = []
    ->  C = R
    ;   rb_delete_all(R, A, RN),
        rolls_cleared(RN, C)
    ).

rolls_count(R, C) :-
    rb_visit(R, Pairs),
    findall('@', member(_-'@', Pairs), Found),
    length(Found, C).   
    
char_one_of(C, L) -->
    [Code],
    {   char_code(C, Code),
        once(member(C, L))
    }.

rolls_data(Rolls-Rolls, _, _) --> eos, !.
    
rolls_data(Rolls0-RollsN, Col, Row) -->
    char_one_of(Char, ['.', '@']), !,
    {   write(Char),
        rb_insert(Rolls0, Col-Row, Char, Rolls1),
        Next is Col + 1
    },!,
    rolls_data(Rolls1-RollsN, Next, Row).

rolls_data(Rolls0-RollsN, _Col, Row) -->
    blanks_to_nl, !,
    {   nl, Next is Row + 1 },
    rolls_data(Rolls0-RollsN, 0, Next).
    

rolls_data_(Rolls,Col,Row) -->
    {writeln(fail:rolls_data(Rolls, Col, Row))}. 
