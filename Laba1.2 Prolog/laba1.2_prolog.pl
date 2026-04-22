% --- 2. Знайти максимальний елемент та переставити всі його примірники в початок списку ---

find_max([X], X).
find_max([X|T], Max) :-
    find_max(T, MaxTail),
    (X > MaxTail -> Max = X ; Max = MaxTail).

collect_max([], _, []).
collect_max([H|T], Max, [H|Result]) :-
    H =:= Max, !,
    collect_max(T, Max, Result).
collect_max([_|T], Max, Result) :-
    collect_max(T, Max, Result).
collect_others([], _, []).
collect_others([H|T], Max, Result) :-
    H =:= Max, !,
    collect_others(T, Max, Result).
collect_others([H|T], Max, [H|Result]) :-
    collect_others(T, Max, Result).

append_lists([], L, L).
append_lists([H|T], L, [H|Result]) :-
    append_lists(T, L, Result).

move_max_to_front([], []).
move_max_to_front(List, Result) :-
    find_max(List, Max),
    collect_max(List, Max, MaxElements),
    collect_others(List, Max, OtherElements),
    append_lists(MaxElements, OtherElements, Result).

read_integer(Value) :-
    catch(read(Temp), _, Temp = invalid),
    (   integer(Temp)
    ->  Value = Temp
    ;   write('Error: Please enter an integer (e.g., "5."): '), nl,
        read_integer(Value)
    ).

read_size(Size) :-
    catch(read(Temp), _, Temp = invalid),
    (   integer(Temp), Temp >= 0
    ->  Size = Temp
    ;   write('Error: Enter a non-negative integer (e.g., "3."): '), nl,
        read_size(Size)
    ).

read_elements(0, _, []) :- !.
read_elements(N, Name, [H|T]) :-
    N > 0,
    format('  List ~w element: ', [Name]),
    read_integer(H),
    N1 is N - 1,
    read_elements(N1, Name, T).

get_list(Id, List) :-
    format('Enter size of list ~w: ', [Id]),
    read_size(Size),
    (   Size =:= 0
    ->  write('Error: List cannot be empty!'), nl,
        get_list(Id, List)
    ;   read_elements(Size, Id, List)
    ).

main :-
    write('=== Task: Find max and move all its instances to the front ==='), nl,
    get_list(1, List),
    move_max_to_front(List, Result),
    nl,
    write('Your list: '), write(List), nl,
    write('Result:    '), write(Result), nl.

:- initialization(main).