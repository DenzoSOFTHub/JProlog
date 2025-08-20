% ===================================================================
% TEST DCG SIMPLE - Programma di test per funzionalità DCG
% ===================================================================

% Grammatica per numeri semplici
digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

% Numero come sequenza di cifre
number(N) --> digit(N).
number(N) --> digit(D), number(N1), { N is D * 10 + N1 }.

% Spazi opzionali
spaces --> [].
spaces --> [32], spaces.  % 32 = codice ASCII dello spazio

% Operatori aritmetici
op_plus --> "+".
op_minus --> "-".
op_mult --> "*".
op_div --> "/".

% Espressione aritmetica semplice
expr(N) --> number(N).
expr(plus(E1, E2)) --> expr(E1), spaces, op_plus, spaces, expr(E2).
expr(minus(E1, E2)) --> expr(E1), spaces, op_minus, spaces, expr(E2).
expr(mult(E1, E2)) --> expr(E1), spaces, op_mult, spaces, expr(E2).
expr(div(E1, E2)) --> expr(E1), spaces, op_div, spaces, expr(E2).

% Parentesi per precedenza
expr(E) --> "(", spaces, expr(E), spaces, ")".

% Valutazione di espressioni
eval(N, N) :- number(N).
eval(plus(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 + R2.
eval(minus(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 - R2.
eval(mult(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 * R2.
eval(div(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 / R2.

% Parsing completo di una stringa
parse_and_eval(String, Result) :-
    atom_codes(String, Codes),
    phrase(expr(AST), Codes),
    eval(AST, Result).

% ===================================================================
% QUERY DI TEST - Usa queste query per testare:
% ===================================================================

% Test 1: Parsing di singole cifre
% ?- phrase(digit(D), "5").
% Dovrebbe dare: D = 5

% Test 2: Parsing di numeri multi-cifra  
% ?- phrase(number(N), "123").
% Dovrebbe dare: N = 123

% Test 3: Parsing di espressioni semplici
% ?- phrase(expr(E), "2+3").
% Dovrebbe dare: E = plus(2, 3)

% Test 4: Parsing ed valutazione completa
% ?- parse_and_eval("2+3", R).
% Dovrebbe dare: R = 5

% Test 5: Espressioni con parentesi
% ?- parse_and_eval("(1+2)*3", R).  
% Dovrebbe dare: R = 9

% Test 6: Espressioni complesse
% ?- parse_and_eval("10-2*3", R).
% Dovrebbe dare: R = 4 (se precedenza corretta)