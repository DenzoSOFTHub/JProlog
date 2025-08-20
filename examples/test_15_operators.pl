% ===================================================================
% TEST 15: Operators and Precedence
% ===================================================================
% Tests: Built-in operators, precedence, custom operators (if supported)

% Standard operator usage
test_arithmetic_ops :-
    X is 2 + 3 * 4,           % Should be 14 (precedence)
    Y is (2 + 3) * 4,         % Should be 20 (parentheses)
    Z is 2 ** 3,              % Should be 8 (power, if supported)
    writeln(arithmetic(X, Y, Z)).

test_comparison_ops :-
    (3 > 2 -> writeln('3 > 2 is true'); writeln('3 > 2 is false')),
    (2 =:= 2.0 -> writeln('2 =:= 2.0 is true'); writeln('2 =:= 2.0 is false')),
    (a @< b -> writeln('a @< b is true'); writeln('a @< b is false')),
    (f(a) == f(a) -> writeln('f(a) == f(a) is true'); writeln('f(a) == f(a) is false')).

test_unification_ops :-
    X = f(Y),
    Y = hello,
    writeln(unified(X)),
    (f(hello) = f(hello) -> writeln('unification works'); writeln('unification fails')),
    (f(a) \= f(b) -> writeln('non-unification works'); writeln('non-unification fails')).

% Custom operator definitions (if op/3 is supported)
% :- op(500, yfx, likes).
% :- op(600, xfx, married_to).
% :- op(300, fx, king).

% If custom operators worked, these would be valid:
% john likes mary.
% mary married_to bob.
% king arthur.

% Instead, use regular syntax:
likes_op(john, mary).
married_to_op(mary, bob).  
king_op(arthur).

% Test operator associativity understanding
test_associativity :-
    % Left associative: 10 - 5 - 2 = (10 - 5) - 2 = 3
    X is 10 - 5 - 2,
    % Right associative: 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (if supported)
    % Y is 2 ** 3 ** 2,  % Might not be supported
    writeln(left_assoc(X)).

% Operator precedence tests
test_precedence :-
    X is 2 + 3 * 4,      % Should be 2 + (3 * 4) = 14
    Y is 2 * 3 + 4,      % Should be (2 * 3) + 4 = 10
    Z is 10 / 2 + 3,     % Should be (10 / 2) + 3 = 8
    writeln(precedence(X, Y, Z)).

% Mixed operators
complex_expression(Result) :-
    A = 5,
    B = 3,
    C = 2,
    Result is A + B * C - 1.    % Should be 5 + (3 * 2) - 1 = 10

% Bitwise operators (if supported)
test_bitwise :-
    X is 5 /\ 3,         % Bitwise AND (might not be supported)
    Y is 5 \/ 3,         % Bitwise OR (might not be supported)  
    writeln(bitwise(X, Y)).

% Test queries:
% ?- test_arithmetic_ops.
% ?- test_comparison_ops.
% ?- test_unification_ops.
% ?- test_precedence.
% ?- complex_expression(R).
% ?- test_bitwise.        % Might fail if bitwise not supported