% -*-Prolog-*-
:- module(util,
          [
              op(950, fy, '*'),
              op(950, fy, '@'),
              '@'/1,
              '*'/1,
              countdown/3,
              debug_call/1,
              list_length/2,
              some_of//1,
              some_of//2,
              some_of//3,
              ignore//0,
              any//1,
              trace_dcg//1
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


:- meta_predicate(trace_dcg(0, ?, ?)).
trace_dcg(D) -->
    any(A),
    {   phrase(D, A)
    ->  string_codes(S, A), string_chars(S, C), writeln(phrase(D, C))
    ;   string_codes(S, A), string_chars(S, C), writeln(not:phrase(D, C))
    }.

list_length(Li, Le) :- length(Li, Le).

:- meta_predicate(some_of(3, ?, ?)).
some_of(_) --> [].
some_of(P) -->
    call(P, _),
    some_of(P).

:- meta_predicate(some_of(3, ?, ?, ?)).
some_of(_, []) --> [].
some_of(P, [H|T]) -->
    call(P,H),
    some_of(P, T).

        
:- meta_predicate(some_of(3, ?, ?, ?, ?)).
some_of(_, _, []) --> [].
    
some_of(P, _, [H]) -->
    call(P,H).
        
some_of(P, S, [H|T]) -->
    call(P,H),
    S,
    {   dif(T, []) },
    some_of(P, S, T).

ignore --> [].
ignore --> [_], ignore.

any([]) --> [].
any([H|T]) --> [H], any(T).

:- meta_predicate(in_out_phrase(?, ?, //)).
in_out_phrase(I, O, P) :-
    phrase(P, I, O).

matches_all(Phrases, I, O) :-
    maplist(in_out_phrase(I, O), Phrases).
