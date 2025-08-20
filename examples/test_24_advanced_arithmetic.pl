% ===================================================================
% TEST 24: Advanced Arithmetic and Mathematical Functions
% ===================================================================
% Tests: Trigonometry, logarithms, advanced math functions, precision

% Mathematical constants and functions
test_constants :-
    Pi is pi,
    E is e,
    Pi > 3.14,
    Pi < 3.15,
    E > 2.71,
    E < 2.72.

% Trigonometric functions
test_trigonometry :-
    X is sin(pi/2),
    Y is cos(0),
    Z is tan(pi/4),
    X =:= 1.0,
    Y =:= 1.0,
    abs(Z - 1.0) < 0.0001.

% Logarithmic and exponential functions
test_logarithms :-
    X is exp(1),
    Y is log(X),
    Z is log10(100),
    abs(Y - 1.0) < 0.0001,
    Z =:= 2.0.

% Power and root functions
test_powers :-
    X is 2 ** 3,
    Y is sqrt(16),
    Z is 27 ** (1/3),
    X =:= 8,
    Y =:= 4.0,
    abs(Z - 3.0) < 0.0001.

% Advanced arithmetic operations
test_advanced_ops :-
    X is gcd(48, 18),
    Y is lcm(12, 18),
    Z is factorial(5),
    X =:= 6,
    Y =:= 36,
    Z =:= 120.

% Bitwise operations
test_bitwise :-
    X is 5 /\ 3,
    Y is 5 \/ 3,
    Z is 5 xor 3,
    W is \ 5,
    X =:= 1,
    Y =:= 7,
    Z =:= 6.

% Floating point precision tests
test_precision :-
    X is 0.1 + 0.2,
    Y is 0.3,
    abs(X - Y) < 1e-10.

% Test queries:
% ?- test_constants.
% ?- test_trigonometry.
% ?- test_logarithms.
% ?- test_powers.
% ?- test_advanced_ops.
% ?- test_bitwise.
% ?- test_precision.