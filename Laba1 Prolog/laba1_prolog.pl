% --- 32. Вилучити з першого списку елементи, що входять у другий список тричі. ---

count(_, [], 0).
count(X, [X|T], N) :- 
    !, 
    count(X, T, N1), 
    N is N1 + 1.
count(X, [_|T], N) :- 
    count(X, T, N).

filter([], _, []).
filter([H|T], List2, Result) :-
    count(H, List2, 3), 
    !, 
    filter(T, List2, Result).
filter([H|T], List2, [H|Result]) :-
    filter(T, List2, Result).

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
    format('  List ~w element: ', [Name]),
    read_integer(H),
    N1 is N - 1,
    read_elements(N1, Name, T).

get_list(Id, List) :-
    format('Enter size of list ~w: ', [Id]),
    read_size(Size),
    read_elements(Size, Id, List).


main :-
    write('=== Task: Remove elements from List 1 that appear 3 times in List 2 ==='), nl,
    get_list(1, List1),
    nl,
    get_list(2, List2),
    
    filter(List1, List2, Result),
    
    nl, write('List 1: '), write(List1), nl,
    write('List 2: '), write(List2), nl,
    write('Result: '), write(Result), nl.

% Для запуску в деяких середовищах:
 :- initialization(main).