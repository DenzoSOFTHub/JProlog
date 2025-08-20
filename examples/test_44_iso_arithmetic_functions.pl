% ===================================================================
% TEST 44: ISO Arithmetic Functions - Comprehensive Testing
% ===================================================================
% Tests all ISO Prolog 13211-1 arithmetic functions implemented in Phase 1

% Test 1: Basic Mathematical Functions
test_basic_math :-
    write('Testing basic mathematical functions:'), nl,
    X1 is sqrt(16),
    (X1 =:= 4.0 -> write('✓ sqrt(16) = 4.0') ; write('✗ sqrt(16) failed')), nl,
    
    X2 is abs(-5),
    (X2 =:= 5.0 -> write('✓ abs(-5) = 5.0') ; write('✗ abs(-5) failed')), nl,
    
    X3 is abs(3.5),
    (X3 =:= 3.5 -> write('✓ abs(3.5) = 3.5') ; write('✗ abs(3.5) failed')), nl.

% Test 2: Trigonometric Functions  
test_trigonometric :-
    write('Testing trigonometric functions:'), nl,
    PI_2 is 3.14159265359 / 2,
    
    X1 is sin(PI_2),
    (X1 > 0.999, X1 < 1.001 -> write('✓ sin(π/2) ≈ 1.0') ; write('✗ sin(π/2) failed')), nl,
    
    X2 is cos(0),
    (X2 =:= 1.0 -> write('✓ cos(0) = 1.0') ; write('✗ cos(0) failed')), nl,
    
    PI_4 is 3.14159265359 / 4,
    X3 is tan(PI_4),
    (X3 > 0.999, X3 < 1.001 -> write('✓ tan(π/4) ≈ 1.0') ; write('✗ tan(π/4) failed')), nl.

% Test 3: Inverse Trigonometric Functions
test_inverse_trig :-
    write('Testing inverse trigonometric functions:'), nl,
    
    X1 is asin(1),
    PI_2 is 3.14159265359 / 2,
    (X1 > PI_2 - 0.001, X1 < PI_2 + 0.001 -> 
        write('✓ asin(1) ≈ π/2') ; write('✗ asin(1) failed')), nl,
    
    X2 is acos(0),
    (X2 > PI_2 - 0.001, X2 < PI_2 + 0.001 -> 
        write('✓ acos(0) ≈ π/2') ; write('✗ acos(0) failed')), nl,
    
    X3 is atan(1),
    PI_4 is 3.14159265359 / 4,
    (X3 > PI_4 - 0.001, X3 < PI_4 + 0.001 -> 
        write('✓ atan(1) ≈ π/4') ; write('✗ atan(1) failed')), nl.

% Test 4: Exponential and Logarithmic Functions
test_exp_log :-
    write('Testing exponential and logarithmic functions:'), nl,
    
    X1 is exp(1),
    E is 2.71828182846,
    (X1 > E - 0.001, X1 < E + 0.001 -> 
        write('✓ exp(1) ≈ e') ; write('✗ exp(1) failed')), nl,
    
    X2 is log(E),
    (X2 > 0.999, X2 < 1.001 -> 
        write('✓ log(e) ≈ 1.0') ; write('✗ log(e) failed')), nl,
    
    X3 is sqrt(25),
    (X3 =:= 5.0 -> write('✓ sqrt(25) = 5.0') ; write('✗ sqrt(25) failed')), nl.

% Test 5: Rounding Functions  
test_rounding :-
    write('Testing rounding functions:'), nl,
    
    X1 is floor(3.7),
    (X1 =:= 3.0 -> write('✓ floor(3.7) = 3.0') ; write('✗ floor(3.7) failed')), nl,
    
    X2 is floor(-2.3),
    (X2 =:= -3.0 -> write('✓ floor(-2.3) = -3.0') ; write('✗ floor(-2.3) failed')), nl,
    
    X3 is ceil(3.2),
    (X3 =:= 4.0 -> write('✓ ceil(3.2) = 4.0') ; write('✗ ceil(3.2) failed')), nl,
    
    X4 is round(3.4),
    (X4 =:= 3.0 -> write('✓ round(3.4) = 3.0') ; write('✗ round(3.4) failed')), nl,
    
    X5 is round(3.6),
    (X5 =:= 4.0 -> write('✓ round(3.6) = 4.0') ; write('✗ round(3.6) failed')), nl.

% Test 6: ISO-Specific Functions
test_iso_functions :-
    write('Testing ISO-specific functions:'), nl,
    
    X1 is sign(5.5),
    (X1 =:= 1.0 -> write('✓ sign(5.5) = 1.0') ; write('✗ sign(5.5) failed')), nl,
    
    X2 is sign(-3.2),
    (X2 =:= -1.0 -> write('✓ sign(-3.2) = -1.0') ; write('✗ sign(-3.2) failed')), nl,
    
    X3 is sign(0),
    (X3 =:= 0.0 -> write('✓ sign(0) = 0.0') ; write('✗ sign(0) failed')), nl,
    
    X4 is truncate(3.8),
    (X4 =:= 3.0 -> write('✓ truncate(3.8) = 3.0') ; write('✗ truncate(3.8) failed')), nl,
    
    X5 is truncate(-3.8),
    (X5 =:= -3.0 -> write('✓ truncate(-3.8) = -3.0') ; write('✗ truncate(-3.8) failed')), nl.

% Test 7: Float Part Functions
test_float_parts :-
    write('Testing float part functions:'), nl,
    
    X1 is float_integer_part(3.7),
    (X1 =:= 3.0 -> write('✓ float_integer_part(3.7) = 3.0') ; 
     write('✗ float_integer_part(3.7) failed')), nl,
    
    X2 is float_integer_part(-2.8),
    (X2 =:= -2.0 -> write('✓ float_integer_part(-2.8) = -2.0') ; 
     write('✗ float_integer_part(-2.8) failed')), nl,
    
    X3 is float_fractional_part(3.7),
    (X3 > 0.69, X3 < 0.71 -> write('✓ float_fractional_part(3.7) ≈ 0.7') ; 
     write('✗ float_fractional_part(3.7) failed')), nl.

% Test 8: Mathematical Constants
test_constants :-
    write('Testing mathematical constants:'), nl,
    
    PI_VALUE is pi,
    (PI_VALUE > 3.141, PI_VALUE < 3.142 -> 
        write('✓ pi constant available') ; write('✗ pi constant failed')), nl,
    
    E_VALUE is e,
    (E_VALUE > 2.718, E_VALUE < 2.719 -> 
        write('✓ e constant available') ; write('✗ e constant failed')), nl.

% Test 9: Complex Expressions
test_complex_expressions :-
    write('Testing complex arithmetic expressions:'), nl,
    
    % Test: sqrt(sin²(π/2) + cos²(π/2)) should be 1
    PI_2 is 3.14159265359 / 2,
    SinVal is sin(PI_2),
    CosVal is cos(PI_2),
    X1 is sqrt(SinVal * SinVal + CosVal * CosVal),
    (X1 > 0.999, X1 < 1.001 -> 
        write('✓ sqrt(sin²(π/2) + cos²(π/2)) = 1') ; 
        write('✗ complex expression failed')), nl,
    
    % Test nested functions: abs(sin(-π/6)) should be 0.5  
    Angle is -3.14159265359 / 6,
    X2 is abs(sin(Angle)),
    (X2 > 0.499, X2 < 0.501 -> 
        write('✓ abs(sin(-π/6)) ≈ 0.5') ; 
        write('✗ nested function failed')), nl.

% Test 10: Edge Cases and Error Handling
test_edge_cases :-
    write('Testing edge cases:'), nl,
    
    % Test sqrt of zero
    X1 is sqrt(0),
    (X1 =:= 0.0 -> write('✓ sqrt(0) = 0.0') ; write('✗ sqrt(0) failed')), nl,
    
    % Test log of 1
    X2 is log(1),
    (X2 =:= 0.0 -> write('✓ log(1) = 0.0') ; write('✗ log(1) failed')), nl,
    
    % Test exp of 0
    X3 is exp(0),
    (X3 =:= 1.0 -> write('✓ exp(0) = 1.0') ; write('✗ exp(0) failed')), nl,
    
    write('Edge cases completed'), nl.

% Master test runner
run_arithmetic_tests :-
    write('=== ISO ARITHMETIC FUNCTIONS TEST SUITE ==='), nl,
    write('Testing Phase 1 mathematical function implementations'), nl, nl,
    
    test_basic_math, nl,
    test_trigonometric, nl, 
    test_inverse_trig, nl,
    test_exp_log, nl,
    test_rounding, nl,
    test_iso_functions, nl,
    test_float_parts, nl,
    test_constants, nl,
    test_complex_expressions, nl,
    test_edge_cases, nl,
    
    write('=== ARITHMETIC FUNCTIONS TEST COMPLETED ==='), nl,
    write('All ISO Prolog arithmetic functions have been tested'), nl.

% Individual quick tests for debugging
quick_test_sqrt :- X is sqrt(16), write('sqrt(16) = '), write(X), nl.
quick_test_sin :- X is sin(1.5708), write('sin(1.5708) = '), write(X), nl.  
quick_test_abs :- X is abs(-5), write('abs(-5) = '), write(X), nl.
quick_test_floor :- X is floor(3.7), write('floor(3.7) = '), write(X), nl.
quick_test_sign :- X is sign(-3), write('sign(-3) = '), write(X), nl.