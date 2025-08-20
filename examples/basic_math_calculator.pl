% ===================================================================
% CALCOLATORE MATEMATICO BASICO PER JPROLOG
% ===================================================================
% Versione molto semplificata per garantire massima compatibilità
% ===================================================================

% -------------------------------------------------------------------
% PREDICATO PRINCIPALE
% -------------------------------------------------------------------

% calc(+Lista, -Risultato)
% Calcola il valore di un'espressione matematica semplice
calc([A], A) :-
    number(A).

calc([A, +, B], Risultato) :-
    number(A),
    number(B),
    Risultato is A + B.

calc([A, -, B], Risultato) :-
    number(A),
    number(B),
    Risultato is A - B.

calc([A, *, B], Risultato) :-
    number(A),
    number(B),
    Risultato is A * B.

calc([A, /, B], Risultato) :-
    number(A),
    number(B),
    B =\= 0,
    Risultato is A / B.

% Supporto per precedenza: prima moltiplicazione/divisione
calc([A, +, B, *, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is B * C,
    Risultato is A + Temp.

calc([A, -, B, *, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is B * C,
    Risultato is A - Temp.

calc([A, *, B, +, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A * B,
    Risultato is Temp + C.

calc([A, *, B, -, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A * B,
    Risultato is Temp - C.

calc([A, +, B, /, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    C =\= 0,
    Temp is B / C,
    Risultato is A + Temp.

calc([A, -, B, /, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    C =\= 0,
    Temp is B / C,
    Risultato is A - Temp.

calc([A, /, B, +, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    B =\= 0,
    Temp is A / B,
    Risultato is Temp + C.

calc([A, /, B, -, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    B =\= 0,
    Temp is A / B,
    Risultato is Temp - C.

% Supporto per parentesi semplici
calc(['(', A, +, B, ')'], Risultato) :-
    number(A),
    number(B),
    Risultato is A + B.

calc(['(', A, -, B, ')'], Risultato) :-
    number(A),
    number(B),
    Risultato is A - B.

calc(['(', A, *, B, ')'], Risultato) :-
    number(A),
    number(B),
    Risultato is A * B.

calc(['(', A, /, B, ')'], Risultato) :-
    number(A),
    number(B),
    B =\= 0,
    Risultato is A / B.

% Parentesi con operazione esterna
calc(['(', A, +, B, ')', *, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A + B,
    Risultato is Temp * C.

calc(['(', A, -, B, ')', *, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A - B,
    Risultato is Temp * C.

calc(['(', A, *, B, ')', +, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    Temp is A * B,
    Risultato is Temp + C.

calc(['(', A, /, B, ')', -, C], Risultato) :-
    number(A),
    number(B),
    number(C),
    B =\= 0,
    Temp is A / B,
    Risultato is Temp - C.

% -------------------------------------------------------------------
% PREDICATI DI TEST
% -------------------------------------------------------------------

% test_calc/0
test_calc :-
    writeln('=== TEST CALCOLATORE BASICO ==='),
    nl,
    
    writeln('Test 1: [5] = 5'),
    calc([5], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test 2: [2, +, 3] = 5'),
    calc([2, +, 3], R2),
    format('Risultato: ~w~n', [R2]),
    nl,
    
    writeln('Test 3: [10, -, 4] = 6'),
    calc([10, -, 4], R3),
    format('Risultato: ~w~n', [R3]),
    nl,
    
    writeln('Test 4: [3, *, 4] = 12'),
    calc([3, *, 4], R4),
    format('Risultato: ~w~n', [R4]),
    nl,
    
    writeln('Test 5: [15, /, 3] = 5'),
    calc([15, /, 3], R5),
    format('Risultato: ~w~n', [R5]),
    nl,
    
    writeln('Test 6: [2, +, 3, *, 4] = 14 (precedenza)'),
    calc([2, +, 3, *, 4], R6),
    format('Risultato: ~w~n', [R6]),
    nl,
    
    writeln('Test 7: [(, 2, +, 3, ), *, 4] = 20 (parentesi)'),
    calc(['(', 2, +, 3, ')', *, 4], R7),
    format('Risultato: ~w~n', [R7]),
    nl.

% test_specifico/2
% Testa un'espressione specifica
test_specifico(Espressione, Atteso) :-
    format('Test: ~w = ~w~n', [Espressione, Atteso]),
    (   calc(Espressione, Risultato)
    ->  (   Risultato =:= Atteso
        ->  writeln('✓ PASS')
        ;   format('✗ FAIL: ottenuto ~w~n', [Risultato])
        )
    ;   writeln('✗ FAIL: calcolo fallito')
    ).

% test_tutti/0
test_tutti :-
    writeln('=== SUITE DI TEST COMPLETA ==='),
    nl,
    test_specifico([5], 5),
    test_specifico([2, +, 3], 5),
    test_specifico([10, -, 4], 6),
    test_specifico([3, *, 4], 12),
    test_specifico([15, /, 3], 5),
    test_specifico([2, +, 3, *, 4], 14),
    test_specifico(['(', 2, +, 3, ')', *, 4], 20),
    test_specifico([6, *, 2, +, 1], 13),
    nl,
    writeln('=== TEST COMPLETATI ===').

% -------------------------------------------------------------------
% PREDICATO DI UTILITÀ
% -------------------------------------------------------------------

% demo/0 
% Dimostra l'uso del calcolatore
demo :-
    writeln('=== DEMO CALCOLATORE MATEMATICO ==='),
    nl,
    writeln('Esempi di utilizzo:'),
    writeln('  calc([2, +, 3], R).           % R = 5'),
    writeln('  calc([3, *, 4], R).           % R = 12'),
    writeln('  calc([2, +, 3, *, 4], R).     % R = 14 (precedenza)'),
    writeln('  calc([(, 2, +, 3, ), *, 4], R). % R = 20 (parentesi)'),
    nl,
    writeln('Prova con: test_calc. oppure test_tutti.'),
    nl.

% -------------------------------------------------------------------
% ESEMPI DI UTILIZZO:
% -------------------------------------------------------------------
%
% ?- calc([2, +, 3], R).
% R = 5.
%
% ?- calc([2, +, 3, *, 4], R).
% R = 14.  % Precedenza: 2 + (3 * 4) = 2 + 12 = 14
%
% ?- calc(['(', 2, +, 3, ')', *, 4], R).
% R = 20.  % Parentesi: (2 + 3) * 4 = 5 * 4 = 20
%
% ?- test_calc.
% [Esegue tutti i test di base]
%
% ?- test_tutti.
% [Esegue suite completa di test]
%
% ?- demo.
% [Mostra esempi di utilizzo]
% -------------------------------------------------------------------