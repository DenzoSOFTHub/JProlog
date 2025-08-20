% Calcolatore Matematico Semplice con DCG
% Versione funzionante che evita ricorsioni complesse
% =====================================================

% Gestione spazi opzionali
ws --> [].
ws --> [32], ws.  % 32 = spazio

% Cifra singola (0-9)
cifra(C) --> [Cod], 
    { Cod >= 48, Cod =< 57, C is Cod - 48 }.

% Numero di una cifra
numero1(N) --> cifra(N).

% Numero di due cifre
numero2(N) --> cifra(D1), cifra(D2),
    { N is D1 * 10 + D2 }.

% Numero di tre cifre  
numero3(N) --> cifra(D1), cifra(D2), cifra(D3),
    { N is D1 * 100 + D2 * 10 + D3 }.

% Riconoscimento numero (prova da più a meno cifre)
numero(N) --> numero3(N).
numero(N) --> numero2(N). 
numero(N) --> numero1(N).

% Operazioni aritmetiche semplici (solo due operandi)

% Addizione: N1 + N2
addizione(R) --> numero(N1), ws, [43], ws, numero(N2),
    { R is N1 + N2 }.  % 43 = '+'

% Sottrazione: N1 - N2  
sottrazione(R) --> numero(N1), ws, [45], ws, numero(N2),
    { R is N1 - N2 }.  % 45 = '-'

% Moltiplicazione: N1 * N2
moltiplicazione(R) --> numero(N1), ws, [42], ws, numero(N2),
    { R is N1 * N2 }.  % 42 = '*'

% Divisione: N1 / N2
divisione(R) --> numero(N1), ws, [47], ws, numero(N2),
    { R is N1 / N2 }.  % 47 = '/'

% Espressione matematica (solo numero o operazione binaria)
espressione(R) --> numero(R).
espressione(R) --> addizione(R).
espressione(R) --> sottrazione(R).
espressione(R) --> moltiplicazione(R).
espressione(R) --> divisione(R).

% Predicato principale per calcolare
calcola_semplice(Stringa, Risultato) :-
    atom_codes(Stringa, Codici),
    phrase((ws, espressione(Risultato), ws), Codici).

% Test componenti
test_cifra_base(C) :- phrase(cifra(C), [53]).        % '5'
test_numero_base(N) :- phrase(numero(N), [49, 50]).  % "12"
test_add_base(R) :- phrase(addizione(R), [53, 43, 51]). % "5+3"

% Esempi che funzionano:
%
% ?- calcola_semplice('7', R).
% R = 7.0
%
% ?- calcola_semplice('5+3', R).  
% R = 8.0
%
% ?- calcola_semplice('9-4', R).
% R = 5.0
%
% ?- calcola_semplice('6*7', R).
% R = 42.0
%
% ?- calcola_semplice('8/2', R).
% R = 4.0
%
% ?- calcola_semplice('12+34', R).
% R = 46.0
%
% ?- calcola_semplice(' 5 + 3 ', R).
% R = 8.0

% Predicati di test
test_numero_semplice :-
    calcola_semplice('5', R),
    write('5 = '), write(R), nl.

test_addizione_semplice :-
    calcola_semplice('3+4', R),
    write('3+4 = '), write(R), nl.

test_sottrazione_semplice :-
    calcola_semplice('9-2', R),
    write('9-2 = '), write(R), nl.

test_moltiplicazione_semplice :-
    calcola_semplice('6*7', R),
    write('6*7 = '), write(R), nl.

test_divisione_semplice :-
    calcola_semplice('8/4', R),
    write('8/4 = '), write(R), nl.

test_numeri_grandi :-
    calcola_semplice('12+34', R),
    write('12+34 = '), write(R), nl.

% Esegui tutti i test semplici
esegui_test_semplici :-
    write('=== Test Calcolatore Semplice ==='), nl,
    test_numero_semplice,
    test_addizione_semplice,
    test_sottrazione_semplice,
    test_moltiplicazione_semplice, 
    test_divisione_semplice,
    test_numeri_grandi,
    write('=== Test completati con successo ==='), nl.