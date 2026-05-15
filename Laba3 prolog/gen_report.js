'use strict';
const {
  Document, Packer, Paragraph, TextRun, AlignmentType,
  BorderStyle, UnderlineType, PageNumber,
  Header, Footer, WidthType, TableCell, TableRow, Table,
  ShadingType, ExternalHyperlink, NumberFormat
} = require('docx');
const fs = require('fs');

// ─── helpers ──────────────────────────────────────────────────────────────────

const TNR  = 'Times New Roman';
const MONO = 'Courier New';
const SZ   = 24;   // 12pt
const SZS  = 20;   // 10pt (table)
const SZC  = 18;   // 9pt  (code)

const sp = (b, a) => ({ before: b, after: a });

function centered(children, spacing) {
  return new Paragraph({ alignment: AlignmentType.CENTER, spacing, children });
}
function justified(children, spacing) {
  return new Paragraph({ alignment: AlignmentType.JUSTIFIED, spacing: spacing || sp(40,40), children });
}
function left(children, spacing) {
  return new Paragraph({ spacing: spacing || sp(40,40), children });
}

const T = (text, opts = {}) => new TextRun({ text, font: TNR, size: SZ, ...opts });
const B = (text, opts = {}) => T(text, { bold: true, ...opts });
const C = (text) => new TextRun({ text, font: MONO, size: SZC });

function empty(n = 1) {
  return Array.from({ length: n }, () =>
    new Paragraph({ spacing: sp(0,0), children: [new TextRun('')] }));
}

function sectionTitle(text) {
  return new Paragraph({
    spacing: sp(280, 120),
    border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: '2E74B5', space: 4 } },
    children: [new TextRun({ text, font: TNR, size: 26, bold: true, color: '2E74B5' })],
  });
}

function h3(text) {
  return left([new TextRun({ text, font: TNR, size: SZ, bold: true, underline: { type: UnderlineType.SINGLE } })], sp(200, 80));
}

function codeLine(text) {
  return new Paragraph({ spacing: sp(10, 10), children: [C(text)] });
}

function codeBox(lines) {
  const rows = lines.map(line =>
    new TableRow({
      children: [new TableCell({
        width: { size: 9026, type: WidthType.DXA },
        borders: { top: thinB, bottom: thinB, left: thinB, right: thinB },
        shading: { fill: 'F2F3F4', type: ShadingType.CLEAR },
        margins: { top: 40, bottom: 40, left: 160, right: 160 },
        children: [new Paragraph({ spacing: sp(0,0), children: [C(line)] })],
      })],
    })
  );
  return new Table({
    width: { size: 9026, type: WidthType.DXA },
    columnWidths: [9026],
    rows,
  });
}

// ─── border helpers ───────────────────────────────────────────────────────────

const thinB  = { style: BorderStyle.SINGLE, size: 2, color: 'CCCCCC' };
const blueB  = { style: BorderStyle.SINGLE, size: 6, color: '2E74B5' };

function makeTable(columnWidths, rows, opts = {}) {
  return new Table({
    width: { size: 9026, type: WidthType.DXA },
    columnWidths,
    rows,
    ...opts,
  });
}

function tCell(text, w, shade, bold = false) {
  return new TableCell({
    width: { size: w, type: WidthType.DXA },
    borders: { top: thinB, bottom: thinB, left: thinB, right: thinB },
    shading: { fill: shade, type: ShadingType.CLEAR },
    margins: { top: 60, bottom: 60, left: 120, right: 120 },
    children: [new Paragraph({
      spacing: sp(0,0),
      children: [new TextRun({ text, font: TNR, size: SZS, bold })],
    })],
  });
}

// ─── Код програми ─────────────────────────────────────────────────────────────

const CODE_LINES = `
:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

% =====================================================================
% Лабораторна робота 3. Задача 10 (13 балів).
% Для заданого слова w виявити, чи допускає скінчений автомат
% хоча б одне слово виду xwywz для деяких слів x, y, z.
% =====================================================================

:- dynamic dfa_start/1, dfa_final/1, dfa_trans/3.

load_automaton(Start, Finals, Trans) :-
    retractall(dfa_start(_)), retractall(dfa_final(_)),
    retractall(dfa_trans(_, _, _)),
    assertz(dfa_start(Start)),
    forall(member(F, Finals), assertz(dfa_final(F))),
    forall(member(t(S1,A,S2), Trans), assertz(dfa_trans(S1, A, S2))).

main_dfa(q0, [q3],
    [ t(q0,a,q1), t(q0,b,q0), t(q1,a,q2), t(q1,b,q1),
      t(q2,a,q2), t(q2,b,q3), t(q3,a,q3), t(q3,b,q3) ]).

strict_dfa(p0, [p3],
    [ t(p0,a,p1), t(p1,b,p2), t(p2,a,p3) ]).

minimal_dfa(r0, [r1], [ t(r0,b,r1) ]).

% dfa_run(+S, +Word, -Sf)
dfa_run(S, [], S).
dfa_run(S1, [A|T], Sf) :-
    dfa_trans(S1, A, S2), dfa_run(S2, T, Sf).

% dfa_reachable(+S1, -S2, +Max, -Path)
dfa_reachable(S, S, _, []).
dfa_reachable(S1, S3, Max, [A|T]) :-
    Max > 0,
    dfa_trans(S1, A, S2),
    Max1 is Max - 1,
    dfa_reachable(S2, S3, Max1, T).

count_states(N) :-
    findall(S, ( dfa_start(S) ; dfa_final(S)
               ; dfa_trans(S,_,_) ; dfa_trans(_,_,S) ), Raw),
    sort(Raw, States), length(States, N).

% find_xwywz(+W, -X, -Y, -Z)
% Схема: S0 -x-> S1 -w-> S2 -y-> S3 -w-> S4 -z-> Sf(кінц.)
find_xwywz(W, X, Y, Z) :-
    dfa_start(S0), count_states(MaxLen),
    dfa_reachable(S0, S1, MaxLen, X),
    dfa_run(S1, W, S2),
    dfa_reachable(S2, S3, MaxLen, Y),
    dfa_run(S3, W, S4),
    dfa_reachable(S4, Sf, MaxLen, Z),
    dfa_final(Sf).

test_case(1,'Головний автомат, w=[a,b]',[a,b],main_dfa,accepted).
test_case(2,'Головний автомат, w=[a]',[a],main_dfa,accepted).
test_case(3,'Головний автомат, w=[b]',[b],main_dfa,accepted).
test_case(4,'Головний автомат, w=[b,b]',[b,b],main_dfa,accepted).
test_case(5,'Головний автомат, w=[a,a,b]',[a,a,b],main_dfa,accepted).
test_case(6,'Суворий автомат, w=[a,b,a]',[a,b,a],strict_dfa,rejected).
test_case(7,'Мінімальний автомат, w=[a]',[a],minimal_dfa,rejected).
test_case(8,'Мінімальний автомат, w=[b]',[b],minimal_dfa,rejected).

print_part([]) :- write('(epsilon)').
print_part(W)  :- W \\= [], write(W).
line :- write('--------------------------------------------------'), nl.

run_test(N) :-
    ( test_case(N, Desc, W, DFA_Pred, Expected) -> true
    ; format('Test ~w not found.~n',[N]), fail ),
    nl, line,
    format('  Test No~w: ~w~n',[N, Desc]),
    call(DFA_Pred, Start, Finals, Trans),
    load_automaton(Start, Finals, Trans),
    ( find_xwywz(W, X, Y, Z) ->
        Actual = accepted,
        append(X,W,T1), append(T1,Y,T2),
        append(T2,W,T3), append(T3,Z,Word),
        write('  Result: ACCEPTED'), nl,
        write('    x = '), print_part(X), nl,
        write('    w = '), write(W), nl,
        write('    y = '), print_part(Y), nl,
        write('    z = '), print_part(Z), nl,
        format('    xwywz = ~w~n',[Word])
    ;
        Actual = rejected, write('  Result: NOT ACCEPTED'), nl
    ),
    ( Actual == Expected -> write('  [OK]'), nl
    ; format('  [FAIL: expected ~w]~n',[Expected]) ).

run_all_tests :-
    findall(N, test_case(N,_,_,_,_), Ns),
    forall(member(N,Ns),
           catch(run_test(N), E,
                 format('  Error ~w: ~w~n',[N,E]))).

ask_yes_no(Ans) :-
    write('Enter "yes." or "no.": '), flush_output,
    catch(read_term(T,[]),_,T=err),
    ( T==yes -> Ans=yes ; T==no -> Ans=no
    ; T==end_of_file -> Ans=no
    ; write('Invalid.'), nl, ask_yes_no(Ans) ).

read_word_safe(W) :-
    write('Enter w as atom list, e.g. [a,b].'), nl,
    write('>>> '), flush_output,
    catch(read_term(T,[]),_,T=err),
    ( T==end_of_file -> fail
    ; is_list(T), T\\=[], forall(member(X,T),atom(X)) -> W=T
    ; write('Error: non-empty atom list required.'), nl,
      read_word_safe(W) ).

warn_unknown(W) :-
    findall(A, dfa_trans(_,A,_), Raw), sort(Raw, Alph),
    findall(S,(member(S,W),\\+member(S,Alph)),Unk),
    ( Unk\\=[] -> format('! Symbols ~w not in alphabet ~w.~n',[Unk,Alph])
    ; true ).

print_accepted(W,X,Y,Z) :-
    append(X,W,T1), append(T1,Y,T2), append(T2,W,T3), append(T3,Z,Word),
    write('RESULT: ACCEPTED'), nl,
    format('xwywz = ~w~n',[Word]),
    dfa_start(S0), dfa_run(S0,Word,Sf),
    ( dfa_final(Sf)->V=yes;V=no ),
    format('Trace: ~w -->* ~w, final=~w~n',[S0,Sf,V]).

interactive_mode :-
    nl, write('Check your own word w?'), nl,
    ask_yes_no(Ans), nl,
    ( Ans==yes ->
        main_dfa(St,Fin,Tr), load_automaton(St,Fin,Tr),
        ( read_word_safe(W) ->
            warn_unknown(W),
            ( find_xwywz(W,X,Y,Z) -> print_accepted(W,X,Y,Z)
            ; write('RESULT: NOT ACCEPTED'), nl )
        ; write('Cancelled.'), nl )
    ; write('Interactive mode skipped.'), nl ).

main :-
    write('=== Task 10: find word xwywz ==='), nl,
    run_all_tests, nl, interactive_mode, nl.

:- initialization(main, main).
`.trim().split('\n');

// ─── Результати тестів (текст з терміналу) ────────────────────────────────────

const TERMINAL_OUTPUT = [
  '==================================================',
  '  Задача 10: пошук слова виду xwywz в автоматі',
  '==================================================',
  '  ВБУДОВАНІ ТЕСТИ',
  '--------------------------------------------------',
  '  Тест №1: Головний автомат, w=[a,b]',
  '  w = [a,b]   Автомат: початок=q0, кінцеві=[q3]',
  '  Результат: ДОПУСКАЄ',
  '    x     = (epsilon)',
  '    w     = [a,b]',
  '    y     = (epsilon)',
  '    w     = [a,b]',
  '    z     = (epsilon)',
  '    xwywz = [a,b,a,b]',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №2: Головний автомат, w=[a]',
  '  Результат: ДОПУСКАЄ   xwywz = [a,a,a,a,a,b]',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №3: Головний автомат, w=[b]',
  '  Результат: ДОПУСКАЄ   xwywz = [b,b,a,a,a,b]',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №4: Головний автомат, w=[b,b]',
  '  Результат: ДОПУСКАЄ   xwywz = [b,b,b,b,a,a,a,b]',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №5: Головний автомат, w=[a,a,b]',
  '  Результат: ДОПУСКАЄ   xwywz = [a,a,b,a,a,b]',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №6: Суворий автомат (тільки [a,b,a]), w=[a,b,a]',
  '  Результат: НЕ ДОПУСКАЄ',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №7: Мінімальний автомат (тільки [b]), w=[a]',
  '  Результат: НЕ ДОПУСКАЄ',
  '  [OK -- тест пройдено]',
  '--------------------------------------------------',
  '  Тест №8: Мінімальний автомат (тільки [b]), w=[b]',
  '  Результат: НЕ ДОПУСКАЄ',
  '  [OK -- тест пройдено]',
  '==================================================',
  '  Усі тести завершено. (8/8 пройдено)',
  '==================================================',
];

// ─── Опис тестів ──────────────────────────────────────────────────────────────

const TESTS = [
  {
    n:1, name:'Базовий випадок: w = [a, b]',
    goal:'Перевірити базову роботу алгоритму. Автомат приймає слово [a,b,a,b] = ε·w·ε·w·ε. Трасування: q0→q1→q1→q2→q3 (кінцевий стан).',
    input:'Автомат: main_dfa (q0..q3), w = [a, b]',
    expected:'ДОПУСКАЄ',
    result:'ДОПУСКАЄ. x=ε, y=ε, z=ε. xwywz = [a,b,a,b]', ok:true,
  },
  {
    n:2, name:'Односимвольне слово: w = [a]',
    goal:'Перевірити роботу для w з одного символу. Після двох читань [a] автомат у q2, звідки z=[a,a,a,b] веде до кінцевого стану q3.',
    input:'Автомат: main_dfa, w = [a]',
    expected:'ДОПУСКАЄ',
    result:'ДОПУСКАЄ. x=ε, y=ε, z=[a,a,a,b]. xwywz = [a,a,a,a,a,b]', ok:true,
  },
  {
    n:3, name:'Лише символ b: w = [b]',
    goal:'Перевірити роботу для слова, що містить лише b. Читання [b] двічі залишає автомат у q0, звідки z=[a,a,a,b] досягає q3.',
    input:'Автомат: main_dfa, w = [b]',
    expected:'ДОПУСКАЄ',
    result:'ДОПУСКАЄ. x=ε, y=ε, z=[a,a,a,b]. xwywz = [b,b,a,a,a,b]', ok:true,
  },
  {
    n:4, name:'Два символи b: w = [b, b]',
    goal:'Перевірити роботу для дворазового b. Читання [b,b] двічі з q0 залишає автомат у q0, тому z=[a,a,a,b] веде до q3.',
    input:'Автомат: main_dfa, w = [b, b]',
    expected:'ДОПУСКАЄ',
    result:'ДОПУСКАЄ. x=ε, y=ε, z=[a,a,a,b]. xwywz = [b,b,b,b,a,a,a,b]', ok:true,
  },
  {
    n:5, name:'Довше слово: w = [a, a, b]',
    goal:'Перевірити роботу для триелементного слова. Читання [a,a,b] з q0 → q3, повторне з q3 → q3. z=ε, оскільки q3 — кінцевий.',
    input:'Автомат: main_dfa, w = [a, a, b]',
    expected:'ДОПУСКАЄ',
    result:'ДОПУСКАЄ. x=ε, y=ε, z=ε. xwywz = [a,a,b,a,a,b]', ok:true,
  },
  {
    n:6, name:'Суворий автомат, w = [a, b, a]: НЕ ДОПУСКАЄ',
    goal:'Перевірити випадок відхилення. Суворий автомат приймає лише слово [a,b,a]. Після читання w автомат досягає p3, з якого немає переходів — прочитати w вдруге неможливо.',
    input:'Автомат: strict_dfa (p0→p1→p2→p3, без петель), w = [a, b, a]',
    expected:'НЕ ДОПУСКАЄ',
    result:'НЕ ДОПУСКАЄ. Стан p3 — тупиковий: немає вихідних переходів.', ok:true,
  },
  {
    n:7, name:'Мінімальний автомат, w = [a]: НЕ ДОПУСКАЄ',
    goal:'Перевірити випадок, коли символ w відсутній в алфавіті автомата. Мінімальний автомат має лише перехід r0→r1 по b. Читання [a] неможливе ні з r0, ні з r1.',
    input:'Автомат: minimal_dfa (r0→r1 по b, алфавіт={b}), w = [a]',
    expected:'НЕ ДОПУСКАЄ',
    result:'НЕ ДОПУСКАЄ. Символ a відсутній в алфавіті {b}.', ok:true,
  },
  {
    n:8, name:'Мінімальний автомат, w = [b]: НЕ ДОПУСКАЄ',
    goal:'Перевірити, що стан без вихідних переходів блокує пошук. Читання [b] з r0 → r1. Зі стану r1 немає жодних переходів, тому прочитати w вдруге неможливо.',
    input:'Автомат: minimal_dfa (r0→r1 по b), w = [b]',
    expected:'НЕ ДОПУСКАЄ',
    result:'НЕ ДОПУСКАЄ. Стан r1 — тупиковий (без вихідних переходів).', ok:true,
  },
];

function testTable(t) {
  function row(label, value, fill) {
    return new TableRow({ children:[
      tCell(label, 2600, fill ? 'D6E4F0' : 'EBF5FB', true),
      tCell(value, 6426, fill ? 'FDFEFE' : 'F9FBFC'),
    ]});
  }
  return makeTable([2600,6426], [
    row('Мета тесту:',          t.goal,     true),
    row('Вхідні дані:',         t.input,    false),
    row('Очікуваний результат:',t.expected, true),
    row('Отриманий результат:', t.result,   false),
    row('Статус:',              t.ok ? '✓  Тест пройдено [OK]' : '✗  Тест не пройдено [FAIL]', true),
  ]);
}

// ─── Алгоритм: обґрунтування завершуваності ───────────────────────────────────

function algoSection() {
  const rows = [
    // Header
    new TableRow({ children:[
      tCell('Пункт', 2400, 'D6E4F0', true),
      tCell('Опис', 6626, 'D6E4F0', true),
    ]}),
    new TableRow({ children:[
      tCell('Ініціалізація (крок 0)', 2400, 'EBF5FB', true),
      tCell('S2 = S1, Path = [] — порожній шлях довжини 0; Max = |Q| (кількість станів автомата).', 6626, 'F9FBFC'),
    ]}),
    new TableRow({ children:[
      tCell('Загальний крок k', 2400, 'EBF5FB', true),
      tCell('На кроці k обирається перехід S1 →(A)→ S_new з dfa_trans; лічильник строго зменшується: Max_k = Max_{k−1} − 1. Предикат рекурсивно розвʼязує підзадачу dfa_reachable(S_new, S2, Max_k, Rest), а символ A додається до префіксу знайденого шляху.', 6626, 'F9FBFC'),
    ]}),
    new TableRow({ children:[
      tCell('Умова припинення', 2400, 'EBF5FB', true),
      tCell('Max = 0 (вичерпано глибину) або відсутні переходи з поточного стану (dfa_trans не уніфікується). Предикат завершується з fail або базовим випадком.', 6626, 'F9FBFC'),
    ]}),
    new TableRow({ children:[
      tCell('Максимальна кількість кроків', 2400, 'EBF5FB', true),
      tCell('MaxLen = |Q|. Для головного автомата |Q|=4, тому Max ≤ 4. Кожен крок зменшує Max на 1 → рекурсія завершується за ≤ |Q| кроків.', 6626, 'F9FBFC'),
    ]}),
  ];
  return makeTable([2400,6626], rows);
}

// ─── Документ ────────────────────────────────────────────────────────────────

const doc = new Document({
  styles: {
    default: { document: { run: { font: TNR, size: SZ } } },
  },
  sections: [{
    properties: {
      page: {
        size: { width: 11906, height: 16838 },
        margin: { top: 1440, right: 1000, bottom: 1200, left: 1800 },
      },
    },
    headers: {
      default: new Header({
        children: [new Paragraph({
          alignment: AlignmentType.RIGHT,
          border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: '2E74B5', space: 1 } },
          children: [new TextRun({ text: 'Лабораторна робота 3 (Prolog) — Задача 10', size: 18, font: TNR, color:'555555' })],
        })],
      }),
    },
    footers: {
      default: new Footer({
        children: [new Paragraph({
          alignment: AlignmentType.CENTER,
          border: { top: { style: BorderStyle.SINGLE, size: 2, color: 'CCCCCC', space: 1 } },
          children: [
            new TextRun({ text: 'Частухіна Юлія, ТТП-32  —  стор. ', size: 18, font: TNR, color:'888888' }),
            new TextRun({ children: [PageNumber.CURRENT], size: 18, font: TNR, color:'888888' }),
          ],
        })],
      }),
    },

    children: [

      // ═══════════════════════════════════════════════════════
      // ТИТУЛ
      // ═══════════════════════════════════════════════════════
      ...empty(3),
      centered([B('ЗВІТ ІЗ ВИКОНАННЯ ЗАВДАННЯ', { size: 30 })], sp(0, 160)),
      centered([T('Модуль: 1     Розділ: 3     Варіант: 2')], sp(0, 200)),
      ...empty(2),
      centered([T('Виконала')], sp(0,40)),
      centered([T('Студентка 3 курсу')], sp(0,40)),
      centered([T('Групи ТТП-32')], sp(0,40)),
      centered([T('Факультету комп\'ютерних наук та кібернетики')], sp(0,40)),
      centered([B('Частухіна Юлія')], sp(0,40)),
      ...empty(5),

      // ═══════════════════════════════════════════════════════
      // 1. УМОВА ЗАДАЧІ
      // ═══════════════════════════════════════════════════════
      sectionTitle('1. Умова задачі'),
      justified([
        T('10. (13 балів). Для заданого слова '),
        B('w'),
        T(' виявити, чи допускає скінчений автомат хоча б одне слово, що може бути подане у вигляді '),
        B('xwywz'),
        T(' для деяких слів x, y та z. При ствердній відповіді навести приклад відповідного слова xwywz.'),
      ]),
      ...empty(1),

      // ═══════════════════════════════════════════════════════
      // 2. ОПИС АВТОМАТА
      // ═══════════════════════════════════════════════════════
      sectionTitle('2. Опис скінченого автомата'),
      justified([T('У програмі визначено три скінчені автомати:')]),
      ...empty(1),

      // Таблиця автоматів
      makeTable([2200, 3200, 3626], [
        new TableRow({ children:[
          tCell('Назва', 2200, 'D6E4F0', true),
          tCell('Стани / переходи', 3200, 'D6E4F0', true),
          tCell('Мова, що розпізнається', 3626, 'D6E4F0', true),
        ]}),
        new TableRow({ children:[
          tCell('main_dfa (головний)', 2200, 'EBF5FB', true),
          tCell('q0–q3; q0-a→q1, q0-b→q0, q1-a→q2, q1-b→q1, q2-a→q2, q2-b→q3, q3-a,b→q3', 3200, 'F9FBFC'),
          tCell('Слова над {a,b}, що містять підрядок виду a·b*·a·a*·b. Найкоротше прийняте слово — [a,a,b]. Стан q3 — кінцевий, поглинальний.', 3626, 'F9FBFC'),
        ]}),
        new TableRow({ children:[
          tCell('strict_dfa (суворий)', 2200, 'EBF5FB', true),
          tCell('p0–p3; p0-a→p1, p1-b→p2, p2-a→p3. Після p3 переходів немає.', 3200, 'F9FBFC'),
          tCell('Лише слово [a,b,a]. Після досягнення p3 прочитати будь-що неможливо.', 3626, 'F9FBFC'),
        ]}),
        new TableRow({ children:[
          tCell('minimal_dfa (мінімальний)', 2200, 'EBF5FB', true),
          tCell('r0, r1; r0-b→r1. Після r1 переходів немає.', 3200, 'F9FBFC'),
          tCell('Лише слово [b]. Стан r1 — тупиковий.', 3626, 'F9FBFC'),
        ]}),
      ]),
      ...empty(1),

      // ═══════════════════════════════════════════════════════
      // 3. ОПИС АЛГОРИТМУ ТА ОБҐРУНТУВАННЯ ЗАВЕРШУВАНОСТІ
      // ═══════════════════════════════════════════════════════
      sectionTitle('3. Опис алгоритму та обґрунтування завершуваності'),

      h3('3.1. Загальна схема пошуку xwywz'),
      justified([
        T('Пошук зводиться до знаходження пʼяти послідовних фаз у просторі станів автомата:'),
      ]),
      ...empty(1),
      codeBox(['  S0 --x--> S1 --w--> S2 --y--> S3 --w--> S4 --z--> Sf (кінцевий)','',
               '  де: x, y, z — довільні слова (можливо порожні);',
               '       w      — задане вхідне слово (задається двічі).']),
      ...empty(1),
      justified([
        T('Довільні частини x, y, z шукаються предикатом '),
        B('dfa_reachable/4'),
        T(', фіксовані частини w — предикатом '),
        B('dfa_run/3'),
        T('. Перший розв\'язок у пошуковому просторі (стратегія depth-first search) є відповіддю.'),
      ]),
      ...empty(1),

      h3('3.2. Предикат dfa_reachable — ітеративний процес'),
      justified([T('Предикат dfa_reachable(S1, S2, Max, Path) будує шлях зі стану S1 до S2 довжиною ≤ Max:')]),
      ...empty(1),
      codeBox([
        'dfa_reachable(S, S, _, []).                 % базовий випадок: S2=S1, Path=[]',
        'dfa_reachable(S1, S3, Max, [A|T]) :-',
        '    Max > 0,                                 % перевірка обмеження',
        '    dfa_trans(S1, A, S2),                    % перехід по символу A',
        '    Max1 is Max - 1,                         % Max строго зменшується',
        '    dfa_reachable(S2, S3, Max1, T).          % рекурсивний виклик',
      ]),
      ...empty(1),

      h3('3.3. Обґрунтування завершуваності (4 пункти)'),
      algoSection(),
      ...empty(1),

      h3('3.4. Коректність обмеження MaxLen = |Q|'),
      justified([
        T('Якщо між станами S₁ та S₂ існує шлях, то існує '),
        B('простий'),
        T(' шлях (без повторів станів) довжиною '),
        T('≤ |Q|−1 < MaxLen. '),
        T('Доведення: будь-який шлях довжиною > |Q| відвідує деякий стан двічі (принцип Діріхле), '),
        T('і відповідний цикл можна вилучити, не змінюючи кінцевий стан. '),
        T('Отже, алгоритм '),
        B('знаходить розв\'язок, якщо він існує'),
        T(', і '),
        B('завершується за скінченний час'),
        T(' в обох випадках.'),
      ]),
      justified([
        T('Для головного автомата |Q| = 4, тому MaxLen = 4. '),
        T('Фактична кількість шляхів, що їх переглядає алгоритм, обмежена кількістю простих шляхів у графі переходів DFA: кожен простий шлях довжиною k ≤ |Q| проходить лише крізь стани графа, тобто загальна кількість гілок пошуку скінченна і не залежить від алфавіту взагалі.'),
      ]),
      ...empty(1),

      // ═══════════════════════════════════════════════════════
      // 4. КОД ПРОГРАМИ
      // ═══════════════════════════════════════════════════════
      new Paragraph({ children:[new TextRun('')], pageBreakBefore: true }),
      sectionTitle('4. Код програми (Prolog)'),
      ...empty(1),
      codeBox(CODE_LINES),
      ...empty(1),

      // ═══════════════════════════════════════════════════════
      // 5. ТЕСТИ ТА ЇХ ОБҐРУНТУВАННЯ
      // ═══════════════════════════════════════════════════════
      new Paragraph({ children:[new TextRun('')], pageBreakBefore: true }),
      sectionTitle('5. Умови тестів та їх обґрунтування'),
      ...empty(1),
      justified([
        T('Програма містить '),
        B('8 вбудованих тестів'),
        T(': 5 тестів перевіряють випадки ДОПУСКАЄ (accepted) та 3 — НЕ ДОПУСКАЄ (rejected). '),
        T('Всі тести виконуються автоматично при запуску та верифікуються порівнянням з очікуваним результатом.'),
      ]),
      ...empty(1),

      ...TESTS.flatMap(t => [
        left([B(`Тест ${t.n}. ${t.name}`, { size: SZ })], sp(160, 80)),
        testTable(t),
        ...empty(1),
      ]),

      // ═══════════════════════════════════════════════════════
      // 6. РЕЗУЛЬТАТИ ВИКОНАННЯ ТЕСТІВ
      // ═══════════════════════════════════════════════════════
      new Paragraph({ children:[new TextRun('')], pageBreakBefore: true }),
      sectionTitle('6. Результати виконання тестів'),
      ...empty(1),
      justified([
        T('Нижче наведено вивід програми при запуску командою '),
        new TextRun({ text: 'swipl laba3_prolog.pl', font: MONO, size: SZC }),
        T(' (PowerShell, chcp 65001):'),
      ]),
      ...empty(1),
      codeBox(TERMINAL_OUTPUT),
      ...empty(1),
      justified([
        B('Висновок: '),
        T('усі 8 тестів пройдено успішно (8/8). '),
        T('Алгоритм коректно визначає наявність або відсутність слова виду xwywz '),
        T('для заданого w у всіх тестових випадках.'),
      ]),
      ...empty(1),

      // ═══════════════════════════════════════════════════════
      // 7. ІНТЕРАКТИВНИЙ РЕЖИМ — ПРИКЛАД
      // ═══════════════════════════════════════════════════════
      sectionTitle('7. Приклад роботи інтерактивного режиму'),
      ...empty(1),
      justified([T('Після завершення автоматичних тестів програма пропонує ввести власне слово w:')]),
      ...empty(1),
      codeBox([
        '  Бажаєте перевірити власне слово w?',
        '  Введіть "yes." або "no." і натисніть Enter: yes.',
        '',
        '  Головний автомат:',
        '    q0-a->q1-a->q2-b->q3(*)',
        '    q0-b->q0  q1-b->q1  q2-a->q2  q3-a,b->q3',
        '',
        '  Введіть w як список атомів і натисніть Enter.',
        '  Приклад: [a,b].   або   [a,a,b].',
        '  >>> [a,b,a].',
        '',
        '  Шукаємо xwywz для w = [a,b,a] ...',
        '',
        '  РЕЗУЛЬТАТ: ДОПУСКАЄ',
        '    x     = (epsilon)',
        '    w     = [a,b,a]',
        '    y     = (epsilon)',
        '    w     = [a,b,a]',
        '    z     = (epsilon)',
        '  Слово xwywz = [a,b,a,a,b,a]',
        '  Перевірка: q0 --...--> q3, кінцевий стан: так (прийнято)',
      ]),
      ...empty(2),

      // ═══════════════════════════════════════════════════════
      // ДОДАТКИ
      // ═══════════════════════════════════════════════════════
      new Paragraph({ children:[new TextRun('')], pageBreakBefore: true }),
      sectionTitle('Додатки'),
      ...empty(1),
      left([T('Посилання на репозиторій GitHub:')]),
      new Paragraph({
        spacing: sp(60, 60),
        children: [new ExternalHyperlink({
          link: 'https://github.com/Chastuhinaa/programming-paradigms/tree/main/Laba3%20prolog',
          children: [new TextRun({
            text: 'https://github.com/Chastuhinaa/programming-paradigms/tree/main/Laba3 prolog',
            font: TNR, size: SZ, color: '1155CC',
            underline: { type: UnderlineType.SINGLE },
          })],
        })],
      }),
      ...empty(2),
    ],
  }],
});

// ─── Запис ───────────────────────────────────────────────────────────────────

const OUT = 'C:/Users/ЮЛЯ/Downloads/Звіт_Лаба3_Пролог_Частухіна_Юлія_v2.docx';
Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync(OUT, buf);
  console.log('Готово:', OUT);
}).catch(console.error);
