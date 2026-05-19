% Лабораторна робота: обчислення follow_1(A)
% для контекстно-вільної граматики

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

:- dynamic(rule/2).
:- dynamic(start/1).
:- dynamic(terminal/1).
:- dynamic(nonterminal/1).
:- dynamic(first_nonterminal/2).

clear_grammar :-
    retractall(rule(_,_)),
    retractall(start(_)),
    retractall(terminal(_)),
    retractall(nonterminal(_)),
    retractall(first_nonterminal(_,_)).

check_start :-
    ( start(_)
    -> true
    ;  write('ПОМИЛКА: не задано аксіому граматики (start/1).'), nl, fail
    ).

check_start_is_nonterminal :-
    start(S),
    ( nonterminal(S)
    -> true
    ;  format("ПОМИЛКА: аксіома ~w не є нетерміналом.~n", [S]), fail
    ).

check_has_rules :-
    ( rule(_, _)
    -> true
    ;  write('ПОМИЛКА: граматика не містить жодного правила.'), nl, fail
    ).

check_lhs_nonterminal :-
    findall(L,
            ( rule(L, _), \+ nonterminal(L) ),
            Bad),
    ( Bad == []
    -> true
    ;  format("ПОМИЛКА: ліві частини не є нетерміналами: ~w~n", [Bad]),
       fail
    ).

check_rhs_symbols :-
    findall(Sym,
            ( rule(_, RHS),
              member(Sym, RHS),
              \+ terminal(Sym),
              \+ nonterminal(Sym)
            ),
            Bad),
    list_to_set(Bad, BadSet),
    ( BadSet == []
    -> true
    ;  format("ПОМИЛКА: невідомі символи у правих частинах: ~w~n", [BadSet]),
       fail
    ).

check_terminal_nonterminal_disjoint :-
    findall(X,
            ( terminal(X), nonterminal(X) ),
            Bad),
    ( Bad == []
    -> true
    ;  format("ПОМИЛКА: символи ~w оголошені і як термінали, і як нетермінали.~n",
              [Bad]),
       fail
    ).

check_all_nonterminals_have_rules :-
    findall(N,
            ( nonterminal(N), \+ rule(N, _) ),
            Bad),
    ( Bad == []
    -> true
    ;  format("ПОПЕРЕДЖЕННЯ: нетермінали без правил: ~w~n", [Bad])
    ).

validate_grammar :-
    check_start,
    check_start_is_nonterminal,
    check_has_rules,
    check_terminal_nonterminal_disjoint,
    check_lhs_nonterminal,
    check_rhs_symbols,
    check_all_nonterminals_have_rules.

is_terminal(X) :- terminal(X).
is_nonterminal(X) :- nonterminal(X).

init_first(Pairs) :-
    findall(N-[], nonterminal(N), Pairs).

first_string_approx([], _, [eps]).
first_string_approx([X|_], _, [X]) :-
    is_terminal(X), !.
first_string_approx([X|Rest], Table, Result) :-
    is_nonterminal(X),
    member(X-FirstX, Table),
    ( member(eps, FirstX)
    -> select(eps, FirstX, FirstXNoEps),
       first_string_approx(Rest, Table, FirstRest),
       union(FirstXNoEps, FirstRest, Result)
    ;  Result = FirstX
    ).

step_first_for(A, Table, NewSet) :-
    findall(F,
            ( rule(A, RHS),
              first_string_approx(RHS, Table, F)
            ),
            Lists),
    flatten(Lists, Flat),
    list_to_set(Flat, NewSet).

step_first_all([], _, []).
step_first_all([N-_|Rest], Table, [N-NewSet|RestNew]) :-
    step_first_for(N, Table, NewSet),
    step_first_all(Rest, Table, RestNew).

fixpoint_first(Table, Result) :-
    step_first_all(Table, Table, NewTable),
    ( NewTable == Table
    -> Result = Table
    ;  fixpoint_first(NewTable, Result)
    ).

compute_first(FirstTable) :-
    init_first(Init),
    fixpoint_first(Init, FirstTable).

first_string([], [eps]).
first_string([X|_], [X]) :-
    is_terminal(X), !.
first_string([X|Rest], Result) :-
    is_nonterminal(X),
    first_nonterminal(X, FirstX),
    ( member(eps, FirstX)
    -> select(eps, FirstX, FirstXNoEps),
       first_string(Rest, FirstRest),
       union(FirstXNoEps, FirstRest, Result)
    ;  Result = FirstX
    ).

store_first([]).
store_first([N-S|Rest]) :-
    assertz(first_nonterminal(N, S)),
    store_first(Rest).

init_follow(Pairs) :-
    start(S),
    findall(N-Set,
            ( nonterminal(N),
              ( N == S -> Set = ['$'] ; Set = [] )
            ),
            Pairs).

occurrence(A, B, Beta) :-
    rule(B, RHS),
    append(_, [A|Beta], RHS),
    is_nonterminal(A).

follow_contribution(B, FirstBeta, FollowTable, Result) :-
    ( select(eps, FirstBeta, FirstBetaNoEps)
    -> member(B-FollowB, FollowTable),
       union(FirstBetaNoEps, FollowB, Result)
    ;  Result = FirstBeta
    ).

step_follow_for(A, FollowTable, NewSet) :-
    findall(Contribution,
            ( occurrence(A, B, Beta),
              first_string(Beta, FirstBeta),
              follow_contribution(B, FirstBeta, FollowTable, Contribution)
            ),
            Lists),
    member(A-Old, FollowTable),
    flatten([Old|Lists], Flat),
    list_to_set(Flat, NewSet).

step_follow_all([], _, []).
step_follow_all([N-_|Rest], Table, [N-NewSet|RestNew]) :-
    step_follow_for(N, Table, NewSet),
    step_follow_all(Rest, Table, RestNew).

fixpoint_follow(Table, Result) :-
    step_follow_all(Table, Table, NewTable),
    ( NewTable == Table
    -> Result = Table
    ;  fixpoint_follow(NewTable, Result)
    ).

compute_follow(FollowTable) :-
    compute_first(FirstTable),
    retractall(first_nonterminal(_,_)),
    store_first(FirstTable),
    init_follow(Init),
    fixpoint_follow(Init, FollowTable).

print_grammar :-
    write('--- Граматика ---'), nl,
    ( start(S) -> format("Аксіома: ~w~n", [S]) ; true ),
    findall(N, nonterminal(N), Ns),
    format("Нетермінали: ~w~n", [Ns]),
    findall(T, terminal(T), Ts),
    format("Термінали:   ~w~n", [Ts]),
    write('Правила:'), nl,
    forall(rule(L, R),
           ( ( R == [] -> format("  ~w -> eps~n", [L])
             ; format("  ~w -> ~w~n", [L, R])
             ))).

print_first_table :-
    compute_first(Table),
    nl, write('--- FIRST ---'), nl,
    forall(member(N-S, Table),
           format("  first(~w) = ~w~n", [N, S])).

print_follow_table :-
    compute_follow(Table),
    nl, write('--- FOLLOW ---'), nl,
    forall(member(N-S, Table),
           format("  follow(~w) = ~w~n", [N, S])).

solve_and_print :-
    ( validate_grammar
    -> print_grammar,
       print_first_table,
       print_follow_table
    ;  write('Граматика некоректна, обчислення неможливе.'), nl
    ).

load_test1 :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')), assertz(nonterminal('A')),
    assertz(nonterminal('B')),
    assertz(terminal(a)), assertz(terminal(b)), assertz(terminal(c)),
    assertz(rule('S', ['A', 'B'])),
    assertz(rule('A', [a, 'A'])),
    assertz(rule('A', [])),
    assertz(rule('B', [b, 'B'])),
    assertz(rule('B', [c])).

load_test2 :-
    clear_grammar,
    assertz(start('E')),
    assertz(nonterminal('E')), assertz(nonterminal('Ep')),
    assertz(nonterminal('T')), assertz(nonterminal('Tp')),
    assertz(nonterminal('F')),
    assertz(terminal(+)), assertz(terminal(*)),
    assertz(terminal('(')), assertz(terminal(')')),
    assertz(terminal(id)),
    assertz(rule('E',  ['T', 'Ep'])),
    assertz(rule('Ep', [+, 'T', 'Ep'])),
    assertz(rule('Ep', [])),
    assertz(rule('T',  ['F', 'Tp'])),
    assertz(rule('Tp', [*, 'F', 'Tp'])),
    assertz(rule('Tp', [])),
    assertz(rule('F',  ['(', 'E', ')'])),
    assertz(rule('F',  [id])).

load_test3 :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')), assertz(nonterminal('A')),
    assertz(nonterminal('B')),
    assertz(terminal(a)), assertz(terminal(b)), assertz(terminal(c)),
    assertz(rule('S', ['A', 'B', c])),
    assertz(rule('A', [a])),
    assertz(rule('B', [b])).

load_test4 :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')), assertz(nonterminal('A')),
    assertz(terminal(a)), assertz(terminal(b)),
    assertz(rule('S', ['A', 'S'])),
    assertz(rule('S', [b])),
    assertz(rule('A', [a, 'A'])),
    assertz(rule('A', [])).

load_test5 :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')), assertz(nonterminal('A')),
    assertz(nonterminal('B')), assertz(nonterminal('C')),
    assertz(terminal(a)), assertz(terminal(b)), assertz(terminal(c)),
    assertz(rule('S', ['A', 'B', 'C'])),
    assertz(rule('A', [a])),
    assertz(rule('A', [])),
    assertz(rule('B', [b])),
    assertz(rule('B', [])),
    assertz(rule('C', [c])),
    assertz(rule('C', [])).

load_test6_error_unknown_symbol :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')),
    assertz(terminal(a)),
    assertz(rule('S', [a, x])).   

load_test7_error_bad_start :-
    clear_grammar,
    assertz(start('Q')),           
    assertz(nonterminal('S')),
    assertz(terminal(a)),
    assertz(rule('S', [a])).

load_test8_error_no_rules :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')),
    assertz(terminal(a)).

load_test9_error_conflict :-
    clear_grammar,
    assertz(start('S')),
    assertz(nonterminal('S')), assertz(nonterminal('A')),
    assertz(terminal('A')),        
    assertz(terminal(a)),
    assertz(rule('S', ['A'])),
    assertz(rule('A', [a])).

run_test(Name, Goal) :-
    nl,
    format("ТЕСТ: ~w~n", [Name]),
    call(Goal),
    solve_and_print.

run_all_tests :-
    run_test('1. Класична граматика з eps',         load_test1),
    run_test('2. Арифметичні вирази (LL(1))',       load_test2),
    run_test('3. Граматика без eps-правил',         load_test3),
    run_test('4. Рекурсивна з eps',                 load_test4),
    run_test('5. Кілька нетерміналів виводять eps', load_test5),
    run_test('6. ПОМИЛКА: невідомий символ',        load_test6_error_unknown_symbol),
    run_test('7. ПОМИЛКА: погана аксіома',          load_test7_error_bad_start),
    run_test('8. ПОМИЛКА: немає правил',            load_test8_error_no_rules),
    run_test('9. ПОМИЛКА: конфлікт T/NT',           load_test9_error_conflict),
    nl,
    write('Усі тести завершено.'), nl.
read_term_safe(Term) :-
    catch(read(Term), _, (Term = error)).

ask_yes_no(Prompt, Answer) :-
    write(Prompt),
    read_term_safe(Resp),
    ( (Resp == yes ; Resp == y ; Resp == 'YES' ; Resp == 'Yes')
    -> Answer = yes
    ;  (Resp == no  ; Resp == n ; Resp == 'NO'  ; Resp == 'No')
    -> Answer = no
    ;  write('Введіть yes. або no.'), nl,
       ask_yes_no(Prompt, Answer)
    ).

read_symbols_list(List) :-
    write('  (введіть список у форматі [a,b,c]. або []. для пустого)'), nl,
    write('  > '),
    read_term_safe(L),
    ( is_list(L)
    -> List = L
    ;  write('  Невірний формат. Спробуйте ще раз.'), nl,
       read_symbols_list(List)
    ).

read_rules :-
    write('Введення правил.'), nl,
    write('Кожне правило: rule(LHS, [символи]). Наприклад: rule(s, [a, s, b]).'), nl,
    write('Для порожньої правої частини: rule(a, []).'), nl,
    write('Для завершення введення введіть: done.'), nl,
    read_rules_loop.

read_rules_loop :-
    write('  правило> '),
    read_term_safe(T),
    ( T == done
    -> true
    ;  T == error
    -> write('  Помилка читання. Спробуйте ще раз.'), nl,
       read_rules_loop
    ;  T = rule(L, R), is_list(R)
    -> assertz(rule(L, R)),
       format("  додано: ~w -> ~w~n", [L, R]),
       read_rules_loop
    ;  write('  Невірний формат. Очікується rule(LHS, [..]). або done.'), nl,
       read_rules_loop
    ).

declare_symbols :-
    write('Введіть список нетерміналів:'), nl,
    read_symbols_list(NTs),
    forall(member(N, NTs), assertz(nonterminal(N))),
    write('Введіть список терміналів:'), nl,
    read_symbols_list(Ts),
    forall(member(T, Ts), assertz(terminal(T))),
    write('Введіть аксіому (наприклад: s.):'), nl,
    write('  > '),
    read_term_safe(S),
    ( S == error
    -> write('  Помилка читання, повторіть.'), nl, declare_symbols
    ;  assertz(start(S))
    ).

interactive_input :-
    clear_grammar,
    nl, write('=== Введення власної граматики ==='), nl,
    declare_symbols,
    read_rules,
    nl, write('Граматику введено. Запуск аналізу...'), nl,
    solve_and_print.

main :-
    nl,
    write('  ОБЧИСЛЕННЯ FOLLOW_1 ДЛЯ КВ-ГРАМАТИКИ'), nl,
    write('  Лабораторна робота, задача 12'), nl,
    nl,
    nl,
    run_all_tests,
    nl,
    ask_yes_no('Бажаєте ввести власну граматику? (yes./no.): ', Ans),
    ( Ans == yes
    -> interactive_input,
       nl,
       ask_yes_no('Ввести ще одну граматику? (yes./no.): ', Again),
       ( Again == yes -> main_loop ; goodbye )
    ;  goodbye
    ).

main_loop :-
    interactive_input,
    ask_yes_no('Ввести ще одну граматику? (yes./no.): ', Ans),
    ( Ans == yes -> main_loop ; goodbye ).

:- initialization(main).
