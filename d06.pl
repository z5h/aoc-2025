% -*-Prolog-*-
:- module(d06,
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
    ->  F = 'input/06/sample.txt'
    ;   F = 'input/06/input.txt'
    ).

a :-
    input_file(Input),
    phrase_from_file(
        (sequence(numbers_row, Rows), operator_row(Ops)),
        Input),
    transpose(Rows, Columns),
    maplist(numbers_op_result, Columns, Ops, Results),
    sumlist(Results, Result),
    format("Result is ~10r", [Result]).

b :-
    input_file(Input),
    phrase_from_file(
        transposed_phrase(
            sequence(cephalopod_expression, Values)
        ),
        Input
    ),
    sumlist(Values, Result),
    format("Result is ~10r", [Result]).

cephalopod_expression(E) -->
    blanks,
    integer(N),
    blanks,
    one_of(Op, [+,*]),
    blanks_to_nl,
    some_of(number_row, Ns),
    blanks_to_nl,
    {   numbers_op_result([N|Ns], Op, E) }.


op_number_t0_t1(Op, N, T0, T1) :- T1 =.. [Op, N, T0].
numbers_op_result([N|Ns], Op, R) :-
    foldl(op_number_t0_t1(Op), Ns, N, Equation),
    R is Equation.

number_row(N) -->
    blanks,
    integer(N),
    blanks_to_nl.
    
numbers_row([H|T]) -->
    blanks,
    integer(H),
    blanks,
    numbers_row(T).

numbers_row([]) -->
    blanks_to_nl.

operator_row([H|T]) -->
    blanks,
    one_of(H, [*, +]),
    operator_row(T).

operator_row([]) -->
    blanks_to_nl.

one_of(C, Chars) -->
    [Code],
    {   char_code(C, Code),
        memberchk(C, Chars)
    }.

% below is for transposing raw input
not_nl(C) -->
    {   dif(C, 0'\n)   },
    [C].

row(Row) -->
    some_of(not_nl, Row), eol, !.

rows(Rows) -->
    some_of(row, Rows).

transposed_phrase(Phrase) -->
    rows(Rows),
    eos, !, 
    {   transpose(Rows, Transposed),
        phrase(rows(Transposed), TransposedInput),
        phrase(Phrase, TransposedInput)
    }.
