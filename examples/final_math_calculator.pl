% ===================================================================
% CALCOLATORE MATEMATICO FINALE PER JPROLOG
% ===================================================================
% Versione estremamente semplificata che evita pattern complessi
% ===================================================================

% -------------------------------------------------------------------
% PREDICATI DI CALCOLO BASE
% -------------------------------------------------------------------

% calc(+Lista, -Risultato)
% Versione con unify esplicito per evitare pattern matching problematici

calc(Lista, Risultato) :-
    Lista = [A],
    number(A),
    Risultato = A.

calc(Lista, Risultato) :-
    Lista = [A, Op, B],
    number(A),
    number(B),
    calcola_operazione(A, Op, B, Risultato).

calc(Lista, Risultato) :-
    Lista = [A, Op1, B, Op2, C],
    number(A),
    number(B), 
    number(C),
    calcola_precedenza(A, Op1, B, Op2, C, Risultato).

calc(Lista, Risultato) :-
    Lista = [Par1, A, Op, B, Par2],
    Par1 = '(',
    Par2 = ')',
    number(A),
    number(B),
    calcola_operazione(A, Op, B, Risultato).

calc(Lista, Risultato) :-
    Lista = [Par1, A, Op1, B, Par2, Op2, C],
    Par1 = '(',
    Par2 = ')',
    number(A),
    number(B),
    number(C),
    calcola_operazione(A, Op1, B, Temp),
    calcola_operazione(Temp, Op2, C, Risultato).

% -------------------------------------------------------------------
% PREDICATI DI SUPPORTO
% -------------------------------------------------------------------

% calcola_operazione(+A, +Op, +B, -Risultato)
calcola_operazione(A, Op, B, Risultato) :-
    Op = +,
    Risultato is A + B.

calcola_operazione(A, Op, B, Risultato) :-
    Op = -,
    Risultato is A - B.

calcola_operazione(A, Op, B, Risultato) :-
    Op = *,
    Risultato is A * B.

calcola_operazione(A, Op, B, Risultato) :-
    Op = /,
    B =\= 0,
    Risultato is A / B.

% calcola_precedenza(+A, +Op1, +B, +Op2, +C, -Risultato)
% Gestisce la precedenza degli operatori: prima *, / poi +, -
calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op2 = *,
    Temp is B * C,
    calcola_operazione(A, Op1, Temp, Risultato).

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op2 = /,
    C =\= 0,
    Temp is B / C,
    calcola_operazione(A, Op1, Temp, Risultato).

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = *,
    Temp is A * B,
    calcola_operazione(Temp, Op2, C, Risultato).

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = /,
    B =\= 0,
    Temp is A / B,
    calcola_operazione(Temp, Op2, C, Risultato).

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = +,
    Op2 = +,
    Temp is A + B,
    Risultato is Temp + C.

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = +,
    Op2 = -,
    Temp is A + B,
    Risultato is Temp - C.

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = -,
    Op2 = +,
    Temp is A - B,
    Risultato is Temp + C.

calcola_precedenza(A, Op1, B, Op2, C, Risultato) :-
    Op1 = -,
    Op2 = -,
    Temp is A - B,
    Risultato is Temp - C.

% -------------------------------------------------------------------
% PREDICATI DI TEST
% -------------------------------------------------------------------

% test_base/0
test_base :-
    writeln('=== TEST CALCOLATORE FINALE ==='),
    nl,
    
    writeln('Test 1: [5]'),
    calc([5], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test 2: [2, +, 3]'),
    calc([2, +, 3], R2),
    format('Risultato: ~w~n', [R2]),
    nl,
    
    writeln('Test 3: [10, -, 4]'),
    calc([10, -, 4], R3),
    format('Risultato: ~w~n', [R3]),
    nl,
    
    writeln('Test 4: [3, *, 7]'),
    calc([3, *, 7], R4),
    format('Risultato: ~w~n', [R4]),
    nl,
    
    writeln('Test 5: [12, /, 3]'),
    calc([12, /, 3], R5),
    format('Risultato: ~w~n', [R5]),
    nl.

% test_precedenza/0
test_precedenza :-
    writeln('=== TEST PRECEDENZA OPERATORI ==='),
    nl,
    
    writeln('Test: [2, +, 3, *, 4] = 14'),
    calc([2, +, 3, *, 4], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test: [12, /, 3, +, 1] = 5'),
    calc([12, /, 3, +, 1], R2),
    format('Risultato: ~w~n', [R2]),
    nl.

% test_parentesi/0
test_parentesi :-
    writeln('=== TEST PARENTESI ==='),
    nl,
    
    writeln('Test: [(, 2, +, 3, )] = 5'),
    calc(['(', 2, +, 3, ')'], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test: [(, 2, +, 3, ), *, 4] = 20'),
    calc(['(', 2, +, 3, ')', *, 4], R2),
    format('Risultato: ~w~n', [R2]),
    nl.

% test_tutti_finale/0
test_tutti_finale :-
    test_base,
    test_precedenza,
    test_parentesi,
    writeln('=== TUTTI I TEST COMPLETATI ===').

% -------------------------------------------------------------------
% PREDICATI DI UTILITÀ
% -------------------------------------------------------------------

% aiuto/0
aiuto :-
    writeln('=== AIUTO CALCOLATORE MATEMATICO ==='),
    nl,
    writeln('Utilizzo: calc(Lista, Risultato).'),
    nl,
    writeln('Esempi supportati:'),
    writeln('  calc([5], R).                    % Numero singolo'),
    writeln('  calc([2, +, 3], R).              % Addizione'),
    writeln('  calc([10, -, 4], R).             % Sottrazione'),
    writeln('  calc([3, *, 7], R).              % Moltiplicazione'),
    writeln('  calc([12, /, 3], R).             % Divisione'),
    writeln('  calc([2, +, 3, *, 4], R).        % Precedenza operatori'),
    writeln('  calc([(, 2, +, 3, )], R).        % Parentesi semplici'),
    writeln('  calc([(, 2, +, 3, ), *, 4], R).  % Parentesi con operazione'),
    nl,
    writeln('Test disponibili:'),
    writeln('  test_base.         % Test operazioni base'),
    writeln('  test_precedenza.   % Test precedenza operatori'),
    writeln('  test_parentesi.    % Test parentesi'),
    writeln('  test_tutti_finale. % Tutti i test'),
    nl.

% -------------------------------------------------------------------
% ESEMPI DI UTILIZZO:
% -------------------------------------------------------------------
%
% ?- calc([2, +, 3], R).
% R = 5.
%
% ?- calc([2, +, 3, *, 4], R).
% R = 14.  % Precedenza: 2 + (3 * 4)
%
% ?- calc(['(', 2, +, 3, ')', *, 4], R).
% R = 20.  % Parentesi: (2 + 3) * 4
%
% ?- test_tutti_finale.
% [Esegue tutti i test]
%
% ?- aiuto.
% [Mostra la guida]
% -------------------------------------------------------------------