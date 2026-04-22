% 7. Розбити заданий список на N підсписків, записуючи в ці підсписки елементи по черзі.

split_into_n(List, N, Result) :-
    length(EmptyLists, N),
    maplist(=([]), EmptyLists),
    distribute(List, 0, N, EmptyLists, ReversedLists),
    maplist(reverse, ReversedLists, Result).

distribute([], _, _, Lists, Lists).
distribute([H|T], Index, N, CurrentLists, Result) :-
    NextIndex is (Index + 1) mod N,
    nth0(Index, CurrentLists, OldSublist),
    NewSublist = [H|OldSublist],
    replace_nth0(Index, CurrentLists, NewSublist, UpdatedLists),
    distribute(T, NextIndex, N, UpdatedLists, Result).

replace_nth0(0, [_|T], NewElem, [NewElem|T]).
replace_nth0(Index, [H|T], NewElem, [H|Result]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_nth0(Index1, T, NewElem, Result).

read_list(List) :-
    write('Enter list elements separated by commas in brackets, end with a period (e.g. [1, 2, 3].): '),
    read_term(Term, []),
    parse_list(Term, List).

parse_list([], []) :- !.

parse_list(Term, Term) :-
    is_list(Term), !,
    ( maplist(integer, Term)
    -> true
    ;  write('Error: all elements must be integers!'), nl, fail
    ).

parse_list(_, _) :-
    write('Error: please enter a list in the format [1, 2, 3].'), nl, fail.

safe_read_list(List) :-
    ( catch(read_list(List), _, fail)
    -> true
    ;  write('Please try again.'), nl,
       safe_read_list(List)
    ).

read_n(N) :-
    write('Enter the number of sublists N (a positive integer): '),
    read_term(Term, []),
    ( integer(Term), Term > 0
    -> N = Term
    ;  write('Error: N must be a positive integer!'), nl, fail
    ).

safe_read_n(N) :-
    ( catch(read_n(N), _, fail)
    -> true
    ;  write('Please try again.'), nl,
       safe_read_n(N)
    ).

print_sublists([], _).
print_sublists([H|T], Index) :-
    format('Sublist ~w: ~w~n', [Index, H]),
    NextIndex is Index + 1,
    print_sublists(T, NextIndex).

main :-
    nl,
    write('=== Split a list into N sublists ==='), nl, nl,
    safe_read_list(List),
    safe_read_n(N),
    ( List = []
    -> write('The list is empty - all sublists will be empty.'), nl
    ;  true
    ),
    split_into_n(List, N, Result),
    nl,
    write('Result:'), nl,
    print_sublists(Result, 1).

:- initialization(main, main).