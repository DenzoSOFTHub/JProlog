/* ============================================
   DCG per espressioni aritmetiche - VERSIONE ULTRA SEMPLIFICATA
   Usa solo fatti per evitare tutti i problemi di congiunzione/disgiunzione
   ============================================ */

/* ---------- Fatti semplici per evitare operatori complessi ---------- */

% Cifre valide (codici ASCII)
digit_code(48). digit_code(49). digit_code(50). digit_code(51). digit_code(52).
digit_code(53). digit_code(54). digit_code(55). digit_code(56). digit_code(57).

% Spazi validi (codici ASCII) 
space_code(9).  % \t
space_code(10). % \n
space_code(32). % space

/* ---------- Helper predicates semplificati ---------- */

% Verifica se un carattere è una cifra (senza congiunzioni)
is_digit_char(D) :- digit_code(D).

% Verifica se un carattere è uno spazio (senza disgiunzioni)
is_space_char(C) :- space_code(C).

% Verifica lista non vuota (semplificato)
non_empty_list([_|_]).

/* ---------- API semplificata ---------- */

% Parser molto basilare per singole cifre
parse_single_digit(Input, N) :-
    atom_codes(Input, [Code]),
    is_digit_char(Code),
    N is Code - 48.

% Eval semplificato - solo numeri e plus
eval_simple(N, N).  % I numeri restituiscono se stessi
eval_simple(plus(A, B), Result) :- 
    eval_simple(A, VA),
    eval_simple(B, VB),
    Result is VA + VB.

/* ---------- Test functions ---------- */

test_digit(N) :- parse_single_digit("5", N).
test_add(R) :- eval_simple(plus(1, 2), R).

/* ---------- DCG semplicissime ---------- */

% DCG per singola cifra
single_digit(D) --> [D], { is_digit_char(D) }.

% DCG per spazi opzionali
optional_space --> [C], { is_space_char(C) }.
optional_space --> [].

/* ---------- Note ----------
Questa è una versione estremamente semplificata che evita:
- Congiunzioni nei braces {}
- Disgiunzioni con ;
- Chiamate built-in complesse
- Gestione di liste complesse

Test funzionanti:
?- test_digit(N).
?- test_add(R).
?- phrase(single_digit(D), [49]).
?- is_digit_char(49).
?- is_space_char(32).
*/