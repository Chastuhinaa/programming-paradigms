:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

% =====================================================================
% Лабораторна робота 3. Задача 10 (13 балів).
% Для заданого слова w виявити, чи допускає скінчений автомат
% хоча б одне слово виду xwywz для деяких слів x, y, z.
% При ствердній відповіді навести приклад xwywz.
% =====================================================================

:- dynamic dfa_start/1, dfa_final/1, dfa_trans/3.

load_automaton(Start, Finals, Trans) :-
    retractall(dfa_start(_)),
    retractall(dfa_final(_)),
    retractall(dfa_trans(_, _, _)),
    assertz(dfa_start(Start)),
    forall(member(F,          Finals), assertz(dfa_final(F))),
    forall(member(t(S1,A,S2), Trans),  assertz(dfa_trans(S1, A, S2))).

main_dfa(q0, [q3],
    [ t(q0,a,q1), t(q0,b,q0),
      t(q1,a,q2), t(q1,b,q1),
      t(q2,a,q2), t(q2,b,q3),
      t(q3,a,q3), t(q3,b,q3) ]).

strict_dfa(p0, [p3],
    [ t(p0,a,p1), t(p1,b,p2), t(p2,a,p3) ]).

minimal_dfa(r0, [r1],
    [ t(r0,b,r1) ]).


dfa_run(S, [], S).
dfa_run(S1, [A|T], Sf) :-
    dfa_trans(S1, A, S2),
    dfa_run(S2, T, Sf).

dfa_reachable(S, S, _, []).
dfa_reachable(S1, S3, Max, [A|T]) :-
    Max > 0,
    dfa_trans(S1, A, S2),
    Max1 is Max - 1,
    dfa_reachable(S2, S3, Max1, T).

count_states(N) :-
    findall(S, ( dfa_start(S) ; dfa_final(S)
               ; dfa_trans(S,_,_) ; dfa_trans(_,_,S) ), Raw),
    sort(Raw, States),
    length(States, N).

find_xwywz(W, X, Y, Z) :-
    dfa_start(S0),
    count_states(MaxLen),
    dfa_reachable(S0, S1, MaxLen, X),   
    dfa_run(S1, W, S2),                  
    dfa_reachable(S2, S3, MaxLen, Y),   
    dfa_run(S3, W, S4),                  
    dfa_reachable(S4, Sf, MaxLen, Z),   
    dfa_final(Sf).



test_case(1,
    'Головний автомат, w=[a,b]  (базовий: xwywz=[a,b,a,b])',
    [a,b], main_dfa, accepted).

test_case(2,
    'Головний автомат, w=[a]    (один символ)',
    [a], main_dfa, accepted).

test_case(3,
    'Головний автомат, w=[b]    (лише b; z доповнює)',
    [b], main_dfa, accepted).

test_case(4,
    'Головний автомат, w=[b,b]  (два b)',
    [b,b], main_dfa, accepted).

test_case(5,
    'Головний автомат, w=[a,a,b] (довше слово)',
    [a,a,b], main_dfa, accepted).

test_case(6,
    'Суворий автомат (тільки [a,b,a]), w=[a,b,a]: p3 -- без переходів',
    [a,b,a], strict_dfa, rejected).

test_case(7,
    'Мінімальний автомат (тільки [b]), w=[a]: символ a відсутній',
    [a], minimal_dfa, rejected).

test_case(8,
    'Мінімальний автомат (тільки [b]), w=[b]: r1 -- без переходів',
    [b], minimal_dfa, rejected).

print_part([]) :- write('(epsilon)').
print_part(W)  :- W \= [], write(W).

line :- write('--------------------------------------------------'), nl.

run_test(N) :-
    ( test_case(N, Desc, W, DFA_Pred, Expected) -> true
    ; format('Тест ~w не знайдено.~n', [N]), fail ),
    nl, line,
    format('  Тест №~w: ~w~n', [N, Desc]),
    format('  w = ~w~n', [W]),
    call(DFA_Pred, Start, Finals, Trans),
    load_automaton(Start, Finals, Trans),
    format('  Автомат: початок=~w, кінцеві=~w~n', [Start, Finals]),
    line,
    ( find_xwywz(W, X, Y, Z) ->
        Actual = accepted,
        append(X, W, T1), append(T1, Y, T2),
        append(T2, W, T3), append(T3, Z, Word),
        write('  Результат: ДОПУСКАЄ'), nl,
        write('    x     = '), print_part(X), nl,
        write('    w     = '), write(W),      nl,
        write('    y     = '), print_part(Y), nl,
        write('    w     = '), write(W),      nl,
        write('    z     = '), print_part(Z), nl,
        format('    xwywz = ~w~n', [Word])
    ;
        Actual = rejected,
        write('  Результат: НЕ ДОПУСКАЄ'), nl
    ),
    ( Actual == Expected ->
        write('  [OK -- тест пройдено]'), nl
    ;
        format('  [ПОМИЛКА: очікувалось ~w, отримано ~w]~n',
               [Expected, Actual])
    ).

run_all_tests :-
    nl,
    
    write('               ВБУДОВАНІ ТЕСТИ'), nl,
    
    findall(N, test_case(N,_,_,_,_), Ns),
    forall(member(N, Ns),
           ( catch(run_test(N), Err,
                   format('  Помилка у тесті ~w: ~w~n', [N, Err])) )),
    nl,
    
    write('            Усі тести завершено.'), nl.
   
ask_yes_no(Answer) :-
    write('  Введіть "yes." або "no." і натисніть Enter: '),
    flush_output,
    catch(read_term(T, []), _, T = read_error),
    ( T == yes        -> Answer = yes
    ; T == no         -> Answer = no
    ; T == end_of_file -> Answer = no         
    ; write('  Невірна відповідь. Потрібно: yes.  або  no.'), nl,
      ask_yes_no(Answer)
    ).

read_word_prompt :-
    write('  Введіть w як список атомів і натисніть Enter.'), nl,
    write('  Приклад: [a,b].   або   [a,a,b].'), nl,
    write('  >>> '), flush_output.

validate_word(T, T) :-
    is_list(T), T \= [],
    forall(member(X, T), atom(X)), !.
validate_word(end_of_file, _) :- !,
    write('  Помилка: введення завершилось (EOF). Спробуйте запустити програму знову.'), nl,
    fail.
validate_word([], _) :- !,
    write('  Помилка: слово не може бути порожнім списком.'), nl, fail.
validate_word(T, _) :-
    \+ is_list(T), !,
    format('  Помилка: "~w" не є списком. Формат: [a,b,c].~n', [T]), fail.
validate_word(T, _) :-
    format('  Помилка: усі елементи мають бути атомами: ~w~n', [T]), fail.

read_word_safe(W) :-
    read_word_prompt,
    catch(read_term(T, []), _, T = read_error),
    ( T == end_of_file ->
        write('  Введення завершилось (EOF).'), nl,
        fail                       
    ; validate_word(T, W) ->
        true
    ;
        write('  Спробуйте ще раз.'), nl,
        read_word_safe(W)
    ).

warn_unknown(W) :-
    findall(A, dfa_trans(_, A, _), Raw),
    sort(Raw, Alphabet),
    findall(S, (member(S, W), \+ member(S, Alphabet)), Unknown),
    ( Unknown \= [] ->
        format('  ! Символи ~w відсутні в алфавіті автомата ~w.~n',
               [Unknown, Alphabet]),
        write('    Автомат не зможе їх читати → результат буде НЕ ДОПУСКАЄ.'), nl
    ; true
    ).

print_accepted(W, X, Y, Z) :-
    append(X, W, T1), append(T1, Y, T2),
    append(T2, W, T3), append(T3, Z, Word),
    nl,
    write('  РЕЗУЛЬТАТ: ДОПУСКАЄ'), nl,
    nl,
    write('  Розкладення xwywz:'), nl,
    write('    x     = '), print_part(X), nl,
    write('    w     = '), write(W),      nl,
    write('    y     = '), print_part(Y), nl,
    write('    w     = '), write(W),      nl,
    write('    z     = '), print_part(Z), nl,
    nl,
    format('  Слово xwywz = ~w~n', [Word]),
    nl,
    write('  Перевірка (трасування):'), nl,
    dfa_start(S0),
    dfa_run(S0, Word, Sf),
    ( dfa_final(Sf) -> V = 'так (прийнято)' ; V = 'ні (відхилено)' ),
    format('    ~w --...--> ~w, кінцевий стан: ~w~n', [S0, Sf, V]).

interactive_mode :-
    nl,
    
    write('  Бажаєте перевірити власне слово w?'), nl,
    ask_yes_no(Ans),
    nl,
    ( Ans == yes ->
        line,
        write('  Головний автомат:'), nl,
        write('    q0-a->q1-a->q2-b->q3(*)'), nl,
        write('    q0-b->q0  q1-b->q1  q2-a->q2  q3-a,b->q3'), nl,
        line,
        main_dfa(Start, Finals, Trans),
        load_automaton(Start, Finals, Trans),
        ( read_word_safe(W) ->
            nl,
            warn_unknown(W),
            nl,
            format('  Шукаємо xwywz для w = ~w ...~n', [W]),
            ( find_xwywz(W, X, Y, Z) ->
                print_accepted(W, X, Y, Z)
            ;
                nl,
                write('  РЕЗУЛЬТАТ: НЕ ДОПУСКАЄ'), nl,
                write('  Автомат не допускає жодного слова виду xwywz.'), nl
            )
        ;
            write('  Не вдалося прочитати слово. Завершення.'), nl
        )
    ;
        write('  Інтерактивний режим пропущено.'), nl
    ).
main :-
    nl,
    
    write('  Задача 10: пошук слова виду xwywz в автоматі'), nl,
    
    run_all_tests,
    nl,
    interactive_mode,
    nl.

:- initialization(main, main).
