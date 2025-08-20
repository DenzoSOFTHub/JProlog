% ===================================================================
% TEST 49: ISO Arithmetic Functions (Phase 3)
% ===================================================================
% Complete test suite for ISO 13211-1 arithmetic functions

% Test basic unary functions
test_basic_unary :-
    writeln('Testing basic unary functions:'),
    X1 is abs(-5.7),
    (X1 =:= 5.7 -> writeln('✓ abs(-5.7) = 5.7') ; writeln('✗ abs failed')),
    
    X2 is sign(-3),
    (X2 =:= -1.0 -> writeln('✓ sign(-3) = -1.0') ; writeln('✗ sign failed')),
    
    X3 is sign(0),
    (X3 =:= 0.0 -> writeln('✓ sign(0) = 0.0') ; writeln('✗ sign(0) failed')),
    
    X4 is sign(7),
    (X4 =:= 1.0 -> writeln('✓ sign(7) = 1.0') ; writeln('✗ sign(7) failed')).

% Test rounding functions  
test_rounding_functions :-
    writeln('Testing rounding functions:'),
    X1 is floor(3.9),
    (X1 =:= 3.0 -> writeln('✓ floor(3.9) = 3.0') ; writeln('✗ floor failed')),
    
    X2 is ceiling(3.1),
    (X2 =:= 4.0 -> writeln('✓ ceiling(3.1) = 4.0') ; writeln('✗ ceiling failed')),
    
    X3 is round(3.7),
    (X3 =:= 4.0 -> writeln('✓ round(3.7) = 4.0') ; writeln('✗ round failed')),
    
    X4 is truncate(-3.7),
    (X4 =:= -3.0 -> writeln('✓ truncate(-3.7) = -3.0') ; writeln('✗ truncate failed')),
    
    X5 is truncate(3.7),
    (X5 =:= 3.0 -> writeln('✓ truncate(3.7) = 3.0') ; writeln('✗ truncate(3.7) failed')).

% Test float part functions
test_float_parts :-
    writeln('Testing float part functions:'),
    X1 is float_integer_part(3.7),
    (X1 =:= 3.0 -> writeln('✓ float_integer_part(3.7) = 3.0') ; writeln('✗ float_integer_part failed')),
    
    X2 is float_integer_part(-3.7),
    (X2 =:= -3.0 -> writeln('✓ float_integer_part(-3.7) = -3.0') ; writeln('✗ float_integer_part(-3.7) failed')),
    
    X3 is float_fractional_part(3.7),
    (abs(X3 - 0.7) < 0.0001 -> writeln('✓ float_fractional_part(3.7) ≈ 0.7') ; writeln('✗ float_fractional_part failed')).

% Test trigonometric functions
test_trigonometric :-
    writeln('Testing trigonometric functions:'),
    X1 is sin(0),
    (abs(X1) < 0.0001 -> writeln('✓ sin(0) ≈ 0') ; writeln('✗ sin(0) failed')),
    
    X2 is cos(0),
    (abs(X2 - 1.0) < 0.0001 -> writeln('✓ cos(0) ≈ 1') ; writeln('✗ cos(0) failed')),
    
    X3 is tan(0),
    (abs(X3) < 0.0001 -> writeln('✓ tan(0) ≈ 0') ; writeln('✗ tan(0) failed')).

% Test inverse trigonometric functions
test_inverse_trigonometric :-
    writeln('Testing inverse trigonometric functions:'),
    X1 is asin(0),
    (abs(X1) < 0.0001 -> writeln('✓ asin(0) ≈ 0') ; writeln('✗ asin(0) failed')),
    
    X2 is acos(1),
    (abs(X2) < 0.0001 -> writeln('✓ acos(1) ≈ 0') ; writeln('✗ acos(1) failed')),
    
    X3 is atan(0),
    (abs(X3) < 0.0001 -> writeln('✓ atan(0) ≈ 0') ; writeln('✗ atan(0) failed')).

% Test logarithmic and exponential functions
test_logarithmic :-
    writeln('Testing logarithmic functions:'),
    X1 is log(1),
    (abs(X1) < 0.0001 -> writeln('✓ log(1) ≈ 0') ; writeln('✗ log(1) failed')),
    
    X2 is exp(0),
    (abs(X2 - 1.0) < 0.0001 -> writeln('✓ exp(0) ≈ 1') ; writeln('✗ exp(0) failed')),
    
    X3 is sqrt(16),
    (X3 =:= 4.0 -> writeln('✓ sqrt(16) = 4.0') ; writeln('✗ sqrt(16) failed')),
    
    X4 is sqrt(2),
    (abs(X4 - 1.4142135623730951) < 0.0001 -> writeln('✓ sqrt(2) ≈ 1.414') ; writeln('✗ sqrt(2) failed')).

% Test binary functions
test_binary_functions :-
    writeln('Testing binary functions:'),
    X1 is max(5, 3),
    (X1 =:= 5.0 -> writeln('✓ max(5, 3) = 5.0') ; writeln('✗ max failed')),
    
    X2 is min(5, 3),
    (X2 =:= 3.0 -> writeln('✓ min(5, 3) = 3.0') ; writeln('✗ min failed')),
    
    X3 is 2 ** 3,
    (X3 =:= 8.0 -> writeln('✓ 2 ** 3 = 8.0') ; writeln('✗ ** failed')),
    
    X4 is atan2(1, 1),
    (abs(X4 - 0.7853981633974483) < 0.0001 -> writeln('✓ atan2(1, 1) ≈ π/4') ; writeln('✗ atan2 failed')).

% Test bitwise operations
test_bitwise_operations :-
    writeln('Testing bitwise operations:'),
    X1 is 5 /\ 3,
    (X1 =:= 1.0 -> writeln('✓ 5 /\\ 3 = 1') ; writeln('✗ bitwise AND failed')),
    
    X2 is 5 \/ 3,
    (X2 =:= 7.0 -> writeln('✓ 5 \\/ 3 = 7') ; writeln('✗ bitwise OR failed')),
    
    X3 is xor(5, 3),
    (X3 =:= 6.0 -> writeln('✓ xor(5, 3) = 6') ; writeln('✗ xor failed')).

% Test mathematical constants
test_constants :-
    writeln('Testing mathematical constants:'),
    X1 is pi,
    (abs(X1 - 3.141592653589793) < 0.0001 -> writeln('✓ pi ≈ 3.14159') ; writeln('✗ pi failed')),
    
    X2 is e,
    (abs(X2 - 2.718281828459045) < 0.0001 -> writeln('✓ e ≈ 2.71828') ; writeln('✗ e failed')).

% Test error handling for domain errors
test_domain_errors :-
    writeln('Testing domain error handling:'),
    
    % sqrt of negative number
    catch(
        (X1 is sqrt(-1), writeln('✗ sqrt(-1) should fail')),
        Error1,
        (writeln('✓ sqrt(-1) correctly throws error'), writeln(Error1))
    ),
    
    % log of non-positive number
    catch(
        (X2 is log(0), writeln('✗ log(0) should fail')),
        Error2,
        (writeln('✓ log(0) correctly throws error'), writeln(Error2))
    ),
    
    % asin out of range
    catch(
        (X3 is asin(2), writeln('✗ asin(2) should fail')),
        Error3,
        (writeln('✓ asin(2) correctly throws error'), writeln(Error3))
    ),
    
    % acos out of range  
    catch(
        (X4 is acos(-2), writeln('✗ acos(-2) should fail')),
        Error4,
        (writeln('✓ acos(-2) correctly throws error'), writeln(Error4))
    ).

% Test zero divisor errors
test_zero_divisor_errors :-
    writeln('Testing zero divisor error handling:'),
    
    % Division by zero
    catch(
        (X1 is 5 / 0, writeln('✗ 5 / 0 should fail')),
        Error1,
        (writeln('✓ 5 / 0 correctly throws error'), writeln(Error1))
    ),
    
    % Mod by zero
    catch(
        (X2 is 5 mod 0, writeln('✗ 5 mod 0 should fail')),
        Error2,  
        (writeln('✓ 5 mod 0 correctly throws error'), writeln(Error2))
    ).

% Test edge cases
test_edge_cases :-
    writeln('Testing edge cases:'),
    
    % Very large numbers
    X1 is 10 ** 10,
    (X1 =:= 10000000000.0 -> writeln('✓ 10**10 = 10^10') ; writeln('✗ large power failed')),
    
    % Very small results
    X2 is sin(0.001),
    (abs(X2 - 0.001) < 0.0001 -> writeln('✓ sin(0.001) ≈ 0.001') ; writeln('✗ small sin failed')),
    
    % Zero cases
    X3 is abs(0),
    (X3 =:= 0.0 -> writeln('✓ abs(0) = 0') ; writeln('✗ abs(0) failed')).

% Run all tests
run_all_arithmetic_tests :-
    writeln('=== ISO ARITHMETIC FUNCTIONS TEST SUITE ==='),
    writeln(''),
    test_basic_unary,
    writeln(''),
    test_rounding_functions,
    writeln(''),
    test_float_parts,
    writeln(''),
    test_trigonometric,
    writeln(''),
    test_inverse_trigonometric,
    writeln(''),
    test_logarithmic,
    writeln(''),
    test_binary_functions,
    writeln(''),
    test_bitwise_operations,
    writeln(''),
    test_constants,
    writeln(''),
    test_domain_errors,
    writeln(''),
    test_zero_divisor_errors,
    writeln(''),
    test_edge_cases,
    writeln(''),
    writeln('=== ARITHMETIC FUNCTIONS TESTS COMPLETED ===').

% Test queries:
% ?- run_all_arithmetic_tests.
% ?- test_basic_unary.
% ?- test_domain_errors.
% ?- X is sqrt(25), writeln(X).
% ?- X is ceiling(3.14), writeln(X).