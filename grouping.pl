:- module(grouping,
          [list_grouping/2,
           group_in/3,
           group_in/4,
           grouping_to_groups/2
          ]).

:- use_module(library(rbtrees)).
:- use_module(library(pairs)).

list_grouping(List, Grouping) :-
    pairs_keys_values(Pairs, List, _),
    list_to_rbtree(Pairs, Grouping).

group_in(A, B, Grouping, Bool) :-
    rb_lookup(A, V1, Grouping),
    rb_lookup(B, V2, Grouping),
    (   V1 == V2
    ->  Bool = false
    ;   V1 = V2,
        Bool = true
    ).

group_in(A, B, Grouping) :-
    rb_lookup(A, V, Grouping),
    rb_lookup(B, V, Grouping).

cons(H,T,[H|T]).

grouping_to_groups(Grouping, Groups) :-
    rb_fold(cons, Grouping, [], KVPairs),
    transpose_pairs(KVPairs, GroupValuePairs),
    sort(GroupValuePairs, Sorted),
    group_pairs_by_key(Sorted, GroupGrouped),
    pairs_values(GroupGrouped, Groups).


