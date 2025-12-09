% -*-Prolog-*-
:- module(d08,
          [ a/0
          , b/0
          ]).

% imports
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ordsets)).
:- use_module(util).
:- use_module(grouping).


test_mode :- false.
input_file(F) :-
    (   test_mode
    ->  F = 'input/08/sample.txt'
    ;   F = 'input/08/input.txt'
    ).

% list of sorted tuple(Dist, Box1, Box2)
xyzs_sorted_tuples(XYZs_ord, Sorted) :-
    input_file(Input),
    phrase_from_file((sequence(xyz, XYZs), eos), Input),
    findall(tuple(D, A, B),
        (   member(A, XYZs),
            member(B, XYZs),
            compare(<, A, B),
            distance(A, B, D)
        ),
        Tuples),
    sort(1, @=<, Tuples, Sorted),
    list_to_ord_set(XYZs, XYZs_ord).

a :-
    xyzs_sorted_tuples(XYZs, Sorted),
    list_grouping(XYZs, Grouping),
    length(First1000, 1000),
    append(First1000, _, Sorted),
    foreach(
        member(tuple(_, A, B), First1000),
        group_in(A, B, Grouping)
        ),
    grouping_to_groups(Grouping, Groups),
    maplist(length, Groups, Sizes),
    sort(0, @>=, Sizes, [Size1, Size2, Size3 | _ ]),
    Total is Size1 * Size2 * Size3,
    writeln(total:Total).

b :-
    xyzs_sorted_tuples(XYZs, Sorted),
    length(XYZs, Count),
    RequiredConnections is Count - 1,
    list_grouping(XYZs, Grouping),

    In  = sorted_connectables__grouping__required_connections(Sorted, Grouping, RequiredConnections),
    Out = sorted_connectables__grouping__required_connections(SortedTail, _, _),
    apply_to_until_is(make_connection, In, done, Out),

    append([_, [tuple(_, X1-_-_, X2-_-_)], SortedTail], Sorted),
    Total is X1 * X2,
    
    writeln(total:Total).

done(sorted_connectables__grouping__required_connections(_,_,0)).
make_connection(In, Out) :-
    In  = sorted_connectables__grouping__required_connections(C0, G, R0),
    Out = sorted_connectables__grouping__required_connections(C1, G, R1),
    C0 = [tuple(_, A, B) | C1],
    group_in(A, B, G, NewConnection),
    (   NewConnection
    ->  R1 is R0 - 1
    ;   R1 is R0
    ).

list_length_keyed(Li, Le-Li) :- 
    length(Li, Le).

distance(X1-Y1-Z1, X2-Y2-Z2, D) :-
    D is sqrt((X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2). 

xyz(X-Y-Z) -->
    integer(X), `,`,
    integer(Y), `,`,
    integer(Z), blanks_to_nl.
