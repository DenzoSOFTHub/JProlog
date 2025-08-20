% ===================================================================
% TEST SIMPLE PARSING - Programma Prolog basico per test parsing
% ===================================================================

% Fatti per cifre
digit_code(48, 0).
digit_code(49, 1).
digit_code(50, 2).
digit_code(51, 3).
digit_code(52, 4).
digit_code(53, 5).

% Parsing di una singola cifra
parse_digit([Code|Rest], Digit, Rest) :-
    digit_code(Code, Digit).

% Parsing di un numero semplice (una cifra)
parse_number(Codes, Number, Rest) :-
    parse_digit(Codes, Number, Rest).

% Test di conversione basic
test_conversion :-
    atom_codes('123', Codes),
    parse_number(Codes, N, _),
    write('Parsed number: '), write(N), nl.

% Test aritmetico semplice
test_arithmetic :-
    X is 2 + 3,
    write('2 + 3 = '), write(X), nl.

% Test built-in predicati
test_builtins :-
    atom_length(hello, Len),
    write('Length of hello: '), write(Len), nl.

% Test di unificazione
test_unify :-
    X = 42,
    Y = 42,
    X = Y,
    write('Unification test passed'), nl.

% Predicato per eseguire tutti i test
run_all_tests :-
    write('=== SIMPLE PARSING TESTS ==='), nl,
    test_conversion,
    test_arithmetic, 
    test_builtins,
    test_unify,
    write('=== ALL TESTS COMPLETED ==='), nl.
