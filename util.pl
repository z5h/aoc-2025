% -*-Prolog-*-
:- module(util,
          [
              op(950, fy, '*'),
              op(950, fy, '@'),
              countdown/3,
              debug_call/1,
              list_length/2,
              some_of//1,
              ignore//0,
              any//1
          ]).

:- meta_predicate('*'(0)).
*(_).

:- meta_predicate('@'(0)).
:- det('@'/1).
@(G) :- G.

countdown(From, To, N) :-
    between(To, From, M),
    N is From - M + To.

:- meta_predicate(debug_call(0)).
debug_call(G) :-
    (   G
    *-> writeln(true:G)
    ;   writeln(fail:G)
    ).

list_length(Li, Le) :- length(Li, Le).

:- meta_predicate(some_of(3, ?, ?)).
some_of(_) --> [].
some_of(P) -->
    call(P, _),
    some_of(P).

ignore --> [].
ignore --> [_], ignore.

any([]) --> [].
any([H|T]) --> [H], any(T).

:- meta_predicate(in_out_phrase(?, ?, //)).
in_out_phrase(I, O, P) :-
    phrase(P, I, O).

matches_all(Phrases, I, O) :-
    maplist(in_out_phrase(I, O), Phrases).
