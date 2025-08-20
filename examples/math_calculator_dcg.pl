% ===================================================================
% CALCOLATORE MATEMATICO CON DCG (Definite Clause Grammars)
% ===================================================================
% Programma Prolog che utilizza DCG per parsare e valutare 
% espressioni matematiche con supporto per:
% - Operazioni base: +, -, *, /
% - Parentesi per precedenza
% - Numeri interi e decimali
% - Gestione corretta della precedenza degli operatori
% ===================================================================

% -------------------------------------------------------------------
% PREDICATO PRINCIPALE: calcola/2
% -------------------------------------------------------------------
% calcola(+Espressione, -Risultato)
% Calcola il valore di un'espressione matematica data come lista di token
% 
% Esempio:
% ?- calcola([2, +, 3, *, 4], R).
% R = 14
%
calcola(Espressione, Risultato) :-
    espressione(AST, Espressione, []),
    valuta(AST, Risultato).

% -------------------------------------------------------------------
% PREDICATO DI UTILITÀ: calcola_stringa/2  
% -------------------------------------------------------------------
% calcola_stringa(+StringaEspressione, -Risultato)
% Versione che accetta una stringa e la tokenizza
%
% Esempio:
% ?- calcola_stringa("2 + 3 * 4", R).
% R = 14
%
calcola_stringa(Stringa, Risultato) :-
    tokenizza(Stringa, Tokens),
    calcola(Tokens, Risultato).

% -------------------------------------------------------------------
% GRAMMATICA DCG PER ESPRESSIONI MATEMATICHE
% -------------------------------------------------------------------

% Espressione: gestisce addizione e sottrazione (precedenza più bassa)
espressione(AST) --> 
    termine(T1),
    espressione_resto(T1, AST).

espressione_resto(Acc, AST) -->
    [+],
    termine(T),
    { nuovo_nodo(+, Acc, T, NuovoAcc) },
    espressione_resto(NuovoAcc, AST).

espressione_resto(Acc, AST) -->
    [-],
    termine(T),
    { nuovo_nodo(-, Acc, T, NuovoAcc) },
    espressione_resto(NuovoAcc, AST).

espressione_resto(AST, AST) --> [].

% Termine: gestisce moltiplicazione e divisione (precedenza più alta)
termine(AST) -->
    fattore(F1),
    termine_resto(F1, AST).

termine_resto(Acc, AST) -->
    [*],
    fattore(F),
    { nuovo_nodo(*, Acc, F, NuovoAcc) },
    termine_resto(NuovoAcc, AST).

termine_resto(Acc, AST) -->
    [/],
    fattore(F),
    { nuovo_nodo(/, Acc, F, NuovoAcc) },
    termine_resto(NuovoAcc, AST).

termine_resto(AST, AST) --> [].

% Fattore: gestisce numeri e parentesi
fattore(Numero) -->
    [Numero],
    { number(Numero) }.

fattore(AST) -->
    ['('],
    espressione(AST),
    [')'].

% Supporto per numeri negativi
fattore(AST) -->
    [-],
    fattore(F),
    { nuovo_nodo(-, 0, F, AST) }.

% -------------------------------------------------------------------
% PREDICATI DI SUPPORTO PER COSTRUZIONE AST
% -------------------------------------------------------------------

% nuovo_nodo(+Operatore, +Sinistro, +Destro, -NodoAST)
% Crea un nuovo nodo nell'Abstract Syntax Tree
nuovo_nodo(Op, Left, Right, node(Op, Left, Right)).

% -------------------------------------------------------------------
% VALUTATORE DELL'AST
% -------------------------------------------------------------------

% valuta(+AST, -Risultato)
% Valuta ricorsivamente l'Abstract Syntax Tree
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
    R =\= 0,  % Controllo divisione per zero
    Risultato is L / R.

% -------------------------------------------------------------------
% TOKENIZZATORE SEMPLICE
% -------------------------------------------------------------------

% tokenizza(+Stringa, -Tokens)
% Converte una stringa in una lista di token
% Nota: implementazione semplificata per dimostrazione
tokenizza(Stringa, Tokens) :-
    atom_codes(Stringa, Codes),
    tokenizza_codes(Codes, Tokens).

tokenizza_codes([], []).

tokenizza_codes([32|Rest], Tokens) :-  % Spazio
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([43|Rest], [+|Tokens]) :-  % +
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([45|Rest], [-|Tokens]) :-  % -
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([42|Rest], [*|Tokens]) :-  % *
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([47|Rest], [/|Tokens]) :-  % /
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([40|Rest], ['('|Tokens]) :-  % (
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([41|Rest], [')'|Tokens]) :-  % )
    tokenizza_codes(Rest, Tokens).

tokenizza_codes([C|Rest], [Numero|Tokens]) :-
    C >= 48, C =< 57,  % È una cifra
    leggi_numero([C|Rest], Numero, RestoCodes),
    tokenizza_codes(RestoCodes, Tokens).

% leggi_numero(+Codes, -Numero, -RestoCodes)
% Legge un numero completo dai codici carattere
leggi_numero(Codes, Numero, RestoCodes) :-
    leggi_cifre(Codes, CifreCodes, RestoCodes),
    number_codes(Numero, CifreCodes).

leggi_cifre([C|Rest], [C|CifreRest], RestoCodes) :-
    C >= 48, C =< 57,  % È una cifra
    leggi_cifre(Rest, CifreRest, RestoCodes).

leggi_cifre([C|Rest], [46|CifreRest], RestoCodes) :-  % Punto decimale
    C =:= 46,
    leggi_cifre(Rest, CifreRest, RestoCodes).

leggi_cifre(Codes, [], Codes).

% -------------------------------------------------------------------
% ESEMPI E TEST
% -------------------------------------------------------------------

% test_esempi/0
% Esegue una serie di test per verificare il funzionamento
test_esempi :-
    writeln('=== TEST CALCOLATORE MATEMATICO DCG ==='),
    nl,
    
    % Test 1: Addizione semplice
    writeln('Test 1: 2 + 3'),
    calcola([2, +, 3], R1),
    format('Risultato: ~w~n', [R1]),
    nl,
    
    % Test 2: Precedenza operatori
    writeln('Test 2: 2 + 3 * 4 (dovrebbe essere 14, non 20)'),
    calcola([2, +, 3, *, 4], R2),
    format('Risultato: ~w~n', [R2]),
    nl,
    
    % Test 3: Parentesi
    writeln('Test 3: (2 + 3) * 4'),
    calcola(['(', 2, +, 3, ')', *, 4], R3),
    format('Risultato: ~w~n', [R3]),
    nl,
    
    % Test 4: Divisione
    writeln('Test 4: 15 / 3'),
    calcola([15, /, 3], R4),
    format('Risultato: ~w~n', [R4]),
    nl,
    
    % Test 5: Espressione complessa
    writeln('Test 5: (10 - 4) * 2 + 8 / 4'),
    calcola(['(', 10, -, 4, ')', *, 2, +, 8, /, 4], R5),
    format('Risultato: ~w~n', [R5]),
    nl,
    
    % Test 6: Numero negativo
    writeln('Test 6: -5 + 3'),
    calcola([-, 5, +, 3], R6),
    format('Risultato: ~w~n', [R6]),
    nl.

% -------------------------------------------------------------------
% PREDICATI DI UTILITÀ INTERATTIVA
% -------------------------------------------------------------------

% calcola_interattivo/0
% Modalità interattiva per calcolare espressioni
calcola_interattivo :-
    writeln('=== CALCOLATORE MATEMATICO INTERATTIVO ==='),
    writeln('Inserisci espressioni come liste di token.'),
    writeln('Esempio: [2, +, 3, *, 4]'),
    writeln('Digita "stop." per uscire.'),
    nl,
    loop_interattivo.

loop_interattivo :-
    write('Espressione> '),
    read(Input),
    (   Input = stop
    ->  writeln('Arrivederci!')
    ;   (   calcola(Input, Risultato)
        ->  format('= ~w~n~n', [Risultato])
        ;   writeln('Errore nel calcolo!')
        ),
        loop_interattivo
    ).

% mostra_ast/1
% Visualizza l'Abstract Syntax Tree di un'espressione
mostra_ast(Espressione) :-
    espressione(AST, Espressione, []),
    writeln('Abstract Syntax Tree:'),
    stampa_ast(AST, 0).

stampa_ast(Numero, Livello) :-
    number(Numero),
    tab(Livello),
    format('~w~n', [Numero]).

stampa_ast(node(Op, Left, Right), Livello) :-
    tab(Livello),
    format('~w~n', [Op]),
    NuovoLivello is Livello + 2,
    stampa_ast(Left, NuovoLivello),
    stampa_ast(Right, NuovoLivello).

% -------------------------------------------------------------------
% ESEMPI DI UTILIZZO:
% -------------------------------------------------------------------
% 
% ?- calcola([2, +, 3, *, 4], R).
% R = 14.
%
% ?- calcola(['(', 2, +, 3, ')', *, 4], R).
% R = 20.
%
% ?- mostra_ast([2, +, 3, *, 4]).
% Abstract Syntax Tree:
% +
%   2
%   *
%     3
%     4
%
% ?- test_esempi.
% [Esegue tutti i test automaticamente]
%
% ?- calcola_interattivo.
% [Avvia la modalità interattiva]
% -------------------------------------------------------------------