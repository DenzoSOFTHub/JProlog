/* ============================================
   DCG per espressioni aritmetiche - VERSIONE SEMPLIFICATA FUNZIONANTE
   Aggira il problema dell'unificazione nei corpi delle regole
   ============================================ */

/* ---------- Parser semplificato ---------- */

% Parsing di numeri singoli
parse_number(Input, N) :- 
    atom_codes(Input, [Code]), 
    Code >= 48, 
    Code =< 57,
    N is Code - 48.

% Parser per espressioni molto semplici (solo addizione)
parse_simple_add(Input, Result) :-
    atom_codes(Input, Codes),
    parse_add_expression(Codes, Result).

parse_add_expression([A, 43, B], plus(NA, NB)) :-  % 43 = '+'
    A >= 48, A =< 57,
    B >= 48, B =< 57,
    NA is A - 48,
    NB is B - 48.

/* ---------- Valutatore ---------- */

% Valutazione semplificata - solo per numeri e addizioni
eval_simple(N, N) :- number(N).
eval_simple(plus(A, B), Result) :- 
    eval_simple(A, VA),
    eval_simple(B, VB),
    Result is VA + VB.

/* ---------- Esempi di test funzionanti ---------- */

% Test con numeri singoli
test_number(N) :- parse_number("5", N).

% Test con addizione semplice  
test_addition(Result) :- 
    parse_simple_add("1+2", AST),
    eval_simple(AST, Result).

/* ---------- Fatti per testing ---------- */
digit_code(48, 0).
digit_code(49, 1).  
digit_code(50, 2).
digit_code(51, 3).
digit_code(52, 4).
digit_code(53, 5).

/* ---------- Note ----------
Questo è un parser molto semplificato che evita l'uso di DCG
perché il problema dell'unificazione nei corpi delle regole
impedisce il funzionamento delle regole DCG trasformate.

Per testare:
?- test_number(N).
?- test_addition(R).
?- eval_simple(plus(1, 2), R).

Le DCG complete richiederebbero la risoluzione del bug
nel query solver riguardo all'unificazione nelle regole.
*/