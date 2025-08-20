% ===================================================================
% CALCOLATORE MATEMATICO ULTRA-SEMPLICE PER JPROLOG
% ===================================================================
% Utilizza solo predicati semplici senza pattern matching complesso
% ===================================================================

% Addizione
somma(A, B, Risultato) :-
    number(A),
    number(B),
    Risultato is A + B.

% Sottrazione
sottrazione(A, B, Risultato) :-
    number(A),
    number(B),
    Risultato is A - B.

% Moltiplicazione
moltiplicazione(A, B, Risultato) :-
    number(A),
    number(B),
    Risultato is A * B.

% Divisione
divisione(A, B, Risultato) :-
    number(A),
    number(B),
    B =\= 0,
    Risultato is A / B.

% Calcolo con precedenza: A + B * C = A + (B * C)
calc_precedenza_mul(A, B, C, Risultato) :-
    number(A),
    number(B), 
    number(C),
    Temp is B * C,
    Risultato is A + Temp.

% Calcolo con precedenza: A - B * C = A - (B * C)
calc_precedenza_sub(A, B, C, Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is B * C,
    Risultato is A - Temp.

% Calcolo con parentesi: (A + B) * C
calc_parentesi_mul(A, B, C, Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A + B,
    Risultato is Temp * C.

% Calcolo con parentesi: (A - B) * C
calc_parentesi_sub(A, B, C, Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A - B,
    Risultato is Temp * C.

% -------------------------------------------------------------------
% PREDICATI DI TEST
% -------------------------------------------------------------------

% test_operazioni_base/0
test_operazioni_base :-
    writeln('=== TEST OPERAZIONI BASE ==='),
    nl,
    
    writeln('Test addizione: 2 + 3'),
    somma(2, 3, R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test sottrazione: 10 - 4'),
    sottrazione(10, 4, R2),
    format('Risultato: ~w~n', [R2]),
    nl,
    
    writeln('Test moltiplicazione: 3 * 7'),
    moltiplicazione(3, 7, R3),
    format('Risultato: ~w~n', [R3]),
    nl,
    
    writeln('Test divisione: 15 / 3'),
    divisione(15, 3, R4),
    format('Risultato: ~w~n', [R4]),
    nl.

% test_precedenza/0
test_precedenza :-
    writeln('=== TEST PRECEDENZA ==='),
    nl,
    
    writeln('Test: 2 + 3 * 4 = 14'),
    calc_precedenza_mul(2, 3, 4, R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test: 10 - 2 * 3 = 4'),
    calc_precedenza_sub(10, 2, 3, R2),
    format('Risultato: ~w~n', [R2]),
    nl.

% test_parentesi/0
test_parentesi :-
    writeln('=== TEST PARENTESI ==='),
    nl,
    
    writeln('Test: (2 + 3) * 4 = 20'),
    calc_parentesi_mul(2, 3, 4, R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test: (10 - 4) * 2 = 12'),
    calc_parentesi_sub(10, 4, 2, R2),
    format('Risultato: ~w~n', [R2]),
    nl.

% test_completo/0
test_completo :-
    test_operazioni_base,
    test_precedenza,
    test_parentesi,
    writeln('=== TUTTI I TEST COMPLETATI ===').

% -------------------------------------------------------------------
% DIMOSTRAZIONE INTERATTIVA
% -------------------------------------------------------------------

% demo/0
demo :-
    writeln('=== DEMO CALCOLATORE MATEMATICO ==='),
    nl,
    writeln('Esempi di utilizzo:'),
    nl,
    writeln('Operazioni base:'),
    writeln('  somma(2, 3, R).           % R = 5'),
    writeln('  sottrazione(10, 4, R).    % R = 6'),
    writeln('  moltiplicazione(3, 7, R). % R = 21'),
    writeln('  divisione(15, 3, R).      % R = 5'),
    nl,
    writeln('Precedenza operatori:'),
    writeln('  calc_precedenza_mul(2, 3, 4, R). % 2 + 3*4 = 14'),
    writeln('  calc_precedenza_sub(10, 2, 3, R). % 10 - 2*3 = 4'),
    nl,
    writeln('Parentesi:'),
    writeln('  calc_parentesi_mul(2, 3, 4, R). % (2+3)*4 = 20'),
    writeln('  calc_parentesi_sub(10, 4, 2, R). % (10-4)*2 = 12'),
    nl,
    writeln('Prova con: test_completo.'),
    nl.

% -------------------------------------------------------------------
% ESEMPI DI UTILIZZO:
% -------------------------------------------------------------------
%
% ?- somma(2, 3, R).
% R = 5.
%
% ?- calc_precedenza_mul(2, 3, 4, R).
% R = 14.  % 2 + (3 * 4)
%
% ?- calc_parentesi_mul(2, 3, 4, R).
% R = 20.  % (2 + 3) * 4
%
% ?- test_completo.
% [Esegue tutti i test]
%
% ?- demo.
% [Mostra esempi]
% -------------------------------------------------------------------