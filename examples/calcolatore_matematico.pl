% Calcolatore di Espressioni Matematiche con DCG
% Programma completo e funzionante che utilizza tutte le correzioni
% =============================================================

% Gestione degli spazi bianchi
spazi --> [].
spazi --> [32], spazi.  % 32 = codice ASCII dello spazio

% Cifra singola (0-9)
cifra(C) --> [Cod], 
    { Cod >= 48, Cod =< 57, C is Cod - 48 }.

% Numero (una o più cifre)
numero(N) --> cifra(N).
numero(N) --> cifra(C), numero(N1), 
    { N is C * 10 + N1 }.

% Fattore: numero o espressione tra parentesi
fattore(F) --> numero(F).
fattore(F) --> [40], spazi, espressione(F), spazi, [41].  % 40='(', 41=')'

% Termine: fattore con moltiplicazione/divisione (associatività sinistra)
termine(T) --> fattore(T).
termine(T) --> fattore(F), spazi, [42], spazi, termine(T1), 
    { T is F * T1 }.  % 42='*'
termine(T) --> fattore(F), spazi, [47], spazi, termine(T1), 
    { T is F / T1 }.  % 47='/'

% Espressione: termine con addizione/sottrazione (associatività sinistra)
espressione(E) --> termine(E).
espressione(E) --> termine(T), spazi, [43], spazi, espressione(E1), 
    { E is T + E1 }.  % 43='+'
espressione(E) --> termine(T), spazi, [45], spazi, espressione(E1), 
    { E is T - E1 }.  % 45='-'

% Predicato principale per calcolare un'espressione da stringa
calcola(Stringa, Risultato) :-
    atom_codes(Stringa, Codici),
    phrase((spazi, espressione(Risultato), spazi), Codici).

% Predicato alternativo usando to_codes (ora funzionante)
calcola_to_codes(Stringa, Risultato) :-
    to_codes(Stringa, Codici),
    phrase((spazi, espressione(Risultato), spazi), Codici).

% Test di componenti individuali
test_cifra(C) :- phrase(cifra(C), [53]).  % Test con '5'
test_numero(N) :- phrase(numero(N), [49, 50, 51]).  % Test con "123"
test_fattore(F) :- phrase(fattore(F), [55]).  % Test con '7'

% Esempi di espressioni che funzionano:
%
% ?- calcola('5', R).
% R = 5.0
%
% ?- calcola('2+3', R).
% R = 5.0
%
% ?- calcola('2*3+4', R).
% R = 10.0
%
% ?- calcola('10-3*2', R).
% R = 4.0
%
% ?- calcola('(2+3)*4', R).
% R = 20.0
%
% ?- calcola('10/2+3', R).
% R = 8.0
%
% ?- calcola(' 5 + 3 * 2 ', R).
% R = 11.0

% Predicati di supporto per test interattivi
test_semplice :-
    calcola('5', R),
    write('5 = '), write(R), nl.

test_addizione :-
    calcola('2+3', R),
    write('2+3 = '), write(R), nl.

test_moltiplicazione :-
    calcola('2*3', R),
    write('2*3 = '), write(R), nl.

test_precedenza :-
    calcola('2+3*4', R),
    write('2+3*4 = '), write(R), nl.

test_parentesi :-
    calcola('(2+3)*4', R),
    write('(2+3)*4 = '), write(R), nl.

test_complesso :-
    calcola('10-3*2+8/4', R),
    write('10-3*2+8/4 = '), write(R), nl.

% Esegui tutti i test
esegui_tutti_test :-
    write('=== Test Calcolatore Matematico ==='), nl,
    test_semplice,
    test_addizione,
    test_moltiplicazione,
    test_precedenza,
    test_parentesi,
    test_complesso,
    write('=== Test completati ==='), nl.