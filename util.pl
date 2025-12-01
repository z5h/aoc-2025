% -*-Prolog-*-
:- module(util,
          [
          	debug_call/1,
          	list_length/2,
            ignore//0,
            any//1
          ]).

:- meta_predicate(debug_call(0)).
debug_call(G) :-
	(   G
	*-> writeln(true:G)
	;   writeln(fail:G)
	).

list_length(Li, Le) :- length(Li, Le).

ignore --> [].
ignore --> [_], ignore.

any([]) --> [].
any([H|T]) --> [H], any(T).
