% ===================================================================
% TEST 34: Advanced Operator Definitions
% ===================================================================
% Tests: op/3 directive, custom operators, precedence, associativity

% Define custom operators
:- op(900, xfx, implies).
:- op(800, xfy, and).
:- op(700, xfy, or).
:- op(600, fy, not).
:- op(500, yfx, plus).
:- op(400, yfx, times).
:- op(300, fx, minus).

% Logical operators implementation
X implies Y :- not X or Y.
X and Y :- X, Y.
X or Y :- X ; Y.
not X :- \+ X.

% Arithmetic operators
X plus Y :- Z is X + Y.
X times Y :- Z is X * Y.
minus X :- Y is -X.

% Test logical expressions
test_logic :-
    true implies true,
    true and true,
    true or false,
    not false.

% Set operations with custom operators
:- op(700, xfx, union).
:- op(700, xfx, intersection).
:- op(700, xfx, difference).
:- op(600, fx, complement).

Set1 union Set2 :- append(Set1, Set2, Union), sort(Union, Result).
Set1 intersection Set2 :- findall(X, (member(X, Set1), member(X, Set2)), Result).
Set1 difference Set2 :- findall(X, (member(X, Set1), \+ member(X, Set2)), Result).
complement Set :- universal_set(Universal), universal_set difference Set.

universal_set([1,2,3,4,5,6,7,8,9,10]).

% Matrix operations
:- op(600, xfx, dot).
:- op(500, xfx, cross).
:- op(400, fx, transpose).

Vector1 dot Vector2 :-
    maplist(call, Vector1, V1),
    maplist(call, Vector2, V2),
    zip_multiply(V1, V2, Products),
    sum_list(Products, Result).

zip_multiply([], [], []).
zip_multiply([A|As], [B|Bs], [C|Cs]) :-
    C is A * B,
    zip_multiply(As, Bs, Cs).

% String operations with operators
:- op(600, xfx, concat).
:- op(500, xfx, repeat).
:- op(400, fx, reverse).

String1 concat String2 :-
    atom_concat(String1, String2, Result).

String repeat N :-
    repeat_string(String, N, Result).

repeat_string(_, 0, '').
repeat_string(String, N, Result) :-
    N > 0,
    N1 is N - 1,
    repeat_string(String, N1, Rest),
    atom_concat(String, Rest, Result).

reverse String :-
    atom_chars(String, Chars),
    reverse(Chars, RevChars),
    atom_chars(Result, RevChars).

% Functional composition operators
:- op(400, xfx, compose).
:- op(300, xfx, pipe).

F compose G :-
    call(G, X, Y),
    call(F, Y, Z).

X pipe F :-
    call(F, X, Y).

% List comprehension syntax
:- op(1200, xfx, such_that).
:- op(1100, xfy, in).

Expression such_that Condition :-
    findall(Expression, Condition, Result).

% Test custom operators
test_operators :-
    % Logical
    (true implies false ; true),
    true and (false or true),
    
    % Set operations
    [1,2,3] union [3,4,5],
    [1,2,3] intersection [2,3,4],
    
    % String operations
    hello concat world,
    star repeat 3,
    reverse hello.

% Operator precedence testing
test_precedence :-
    % Should parse as: not (false and true)
    not false and true,
    
    % Should parse as: (true or false) and true
    true or false and true.

% Custom operator for pattern matching
:- op(1100, xfx, matches).

Term matches Pattern :-
    Term = Pattern.

test_matching :-
    f(a, b) matches f(X, Y),
    format('X = ~w, Y = ~w~n', [X, Y]).

% Operator for partial application
:- op(200, fy, curry).

curry Predicate :-
    call(Predicate).

% Test queries:
% ?- test_logic.
% ?- test_operators.
% ?- test_precedence.
% ?- test_matching.
% ?- X such_that (X in [1,2,3,4,5], X > 3).
% ?- current_op(Precedence, Type, implies).