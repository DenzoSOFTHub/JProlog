% ===================================================================
% CALCOLATORE MATEMATICO SEMPLIFICATO PER JPROLOG
% ===================================================================
% Versione semplificata che usa predicati normali invece di DCG
% per garantire compatibilità con JProlog
% ===================================================================

% -------------------------------------------------------------------
% PREDICATO PRINCIPALE: calcola/2
% -------------------------------------------------------------------
% calcola(+Lista, -Risultato)
% Calcola il valore di un'espressione matematica data come lista
calcola(Lista, Risultato) :-
    parse_espressione(Lista, AST, []),
    valuta(AST, Risultato).

% -------------------------------------------------------------------
% PARSER RICORSIVO PER ESPRESSIONI
% -------------------------------------------------------------------

% parse_espressione(+Input, -AST, -Resto)
% Parsing di una espressione (addizione/sottrazione)
parse_espressione(Input, AST, Resto) :-
    parse_termine(Input, T1, Input2),
    parse_espressione_resto(Input2, T1, AST, Resto).

parse_espressione_resto([+|Input], Acc, AST, Resto) :-
    parse_termine(Input, T, Input2),
    nuovo_nodo(+, Acc, T, NuovoAcc),
    parse_espressione_resto(Input2, NuovoAcc, AST, Resto).

parse_espressione_resto([-|Input], Acc, AST, Resto) :-
    parse_termine(Input, T, Input2),
    nuovo_nodo(-, Acc, T, NuovoAcc),
    parse_espressione_resto(Input2, NuovoAcc, AST, Resto).

parse_espressione_resto(Resto, AST, AST, Resto).

% parse_termine(+Input, -AST, -Resto)
% Parsing di un termine (moltiplicazione/divisione)
parse_termine(Input, AST, Resto) :-
    parse_fattore(Input, F1, Input2),
    parse_termine_resto(Input2, F1, AST, Resto).

parse_termine_resto([*|Input], Acc, AST, Resto) :-
    parse_fattore(Input, F, Input2),
    nuovo_nodo(*, Acc, F, NuovoAcc),
    parse_termine_resto(Input2, NuovoAcc, AST, Resto).

parse_termine_resto([/|Input], Acc, AST, Resto) :-
    parse_fattore(Input, F, Input2),
    nuovo_nodo(/, Acc, F, NuovoAcc),
    parse_termine_resto(Input2, NuovoAcc, AST, Resto).

parse_termine_resto(Resto, AST, AST, Resto).

% parse_fattore(+Input, -AST, -Resto)
% Parsing di un fattore (numero o parentesi)
parse_fattore([Numero|Resto], Numero, Resto) :-
    number(Numero).

parse_fattore(['('|Input], AST, Resto) :-
    parse_espressione(Input, AST, [')'|Resto]).

parse_fattore([-|Input], AST, Resto) :-
    parse_fattore(Input, F, Resto),
    nuovo_nodo(-, 0, F, AST).

% -------------------------------------------------------------------
% COSTRUZIONE ABSTRACT SYNTAX TREE
% -------------------------------------------------------------------

% nuovo_nodo(+Op, +Left, +Right, -Node)
nuovo_nodo(Op, Left, Right, node(Op, Left, Right)).

% -------------------------------------------------------------------
% VALUTATORE
% -------------------------------------------------------------------

% valuta(+AST, -Risultato)
valuta(Numero, Numero) :-
    number(Numero).

valuta(node(+, Left, Right), Risultato) :-
    valuta(Left, L),
    valuta(Right, R),
    Risultato is L + R.

valuta(node(-, Left, Right), Risultato) :-
    valuta(Left, L),
    valuta(Right, R),
    Risultato is L - R.

valuta(node(*, Left, Right), Risultato) :-
    valuta(Left, L),
    valuta(Right, R),
    Risultato is L * R.

valuta(node(/, Left, Right), Risultato) :-
    valuta(Left, L),
    valuta(Right, R),
    R =\= 0,
    Risultato is L / R.

% -------------------------------------------------------------------
% ESEMPI E TEST
% -------------------------------------------------------------------

% test_semplici/0
test_semplici :-
    writeln('=== TEST CALCOLATORE SEMPLIFICATO ==='),
    nl,
    
    writeln('Test 1: 2 + 3'),
    calcola([2, +, 3], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    writeln('Test 2: 2 + 3 * 4'),
    calcola([2, +, 3, *, 4], R2),
    format('Risultato: ~w~n', [R2]),
    nl,
    
    writeln('Test 3: (2 + 3) * 4'),
    calcola(['(', 2, +, 3, ')', *, 4], R3),
    format('Risultato: ~w~n', [R3]),
    nl.

% mostra_parsing/1
% Mostra il processo di parsing di un'espressione
mostra_parsing(Lista) :-
    writeln('=== ANALISI PARSING ==='),
    format('Input: ~w~n', [Lista]),
    (   parse_espressione(Lista, AST, [])
    ->  format('AST: ~w~n', [AST]),
        valuta(AST, Risultato),
        format('Risultato: ~w~n', [Risultato])
    ;   writeln('Errore nel parsing')
    ).

% -------------------------------------------------------------------
% ESEMPI DI UTILIZZO:
% -------------------------------------------------------------------
%
% ?- calcola([2, +, 3], R).
% R = 5.
%
% ?- calcola([2, +, 3, *, 4], R).
% R = 14.
%
% ?- calcola(['(', 2, +, 3, ')', *, 4], R).
% R = 20.
%
% ?- mostra_parsing([2, +, 3, *, 4]).
% === ANALISI PARSING ===
% Input: [2, +, 3, *, 4]
% AST: node(+, 2, node(*, 3, 4))
% Risultato: 14
%
% ?- test_semplici.
% [Esegue tutti i test]
% -------------------------------------------------------------------