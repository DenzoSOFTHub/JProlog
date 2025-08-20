% ===================================================================
% PHASE 3: ISO Arithmetic Functions Test
% ===================================================================
% Test current implementation of arithmetic functions

% Basic trigonometric functions
test_trig_functions :-
    writeln('Testing trigonometric functions:'),
    X1 is sin(0),
    writeln(sin_0(X1)),
    X2 is cos(0), 
    writeln(cos_0(X2)),
    X3 is tan(0),
    writeln(tan_0(X3)),
    X4 is sqrt(16),
    writeln(sqrt_16(X4)).

% Test basic arithmetic functions
test_basic_functions :-
    writeln('Testing basic functions:'),
    X1 is abs(-5),
    writeln(abs_neg5(X1)),
    X2 is floor(3.7),
    writeln(floor_3_7(X2)),
    X3 is ceil(3.2),
    writeln(ceil_3_2(X3)),
    X4 is round(3.7),
    writeln(round_3_7(X4)).

% Test logarithmic functions
test_log_functions :-
    writeln('Testing logarithmic functions:'),
    X1 is log(1),
    writeln(log_1(X1)),
    X2 is exp(0),
    writeln(exp_0(X2)).

% Test binary functions  
test_binary_functions :-
    writeln('Testing binary functions:'),
    X1 is max(5, 3),
    writeln(max_5_3(X1)),
    X2 is min(5, 3),
    writeln(min_5_3(X2)),
    X3 is 2 ** 3,
    writeln(power_2_3(X3)).

% Test ISO-specific functions
test_iso_functions :-
    writeln('Testing ISO-specific functions:'),
    X1 is sign(-5),
    writeln(sign_neg5(X1)),
    X2 is sign(0),
    writeln(sign_0(X2)),
    X3 is sign(5),
    writeln(sign_5(X3)),
    X4 is truncate(-3.7),
    writeln(truncate_neg3_7(X4)),
    X5 is truncate(3.7),
    writeln(truncate_3_7(X5)).

% Test bitwise operations
test_bitwise_functions :-
    writeln('Testing bitwise functions:'),
    X1 is 5 /\ 3,
    writeln(bitwise_and_5_3(X1)),
    X2 is 5 \/ 3,
    writeln(bitwise_or_5_3(X2)),
    X3 is xor(5, 3),
    writeln(xor_5_3(X3)).

% Test mathematical constants
test_constants :-
    writeln('Testing constants:'),
    X1 is pi,
    writeln(pi(X1)),
    X2 is e,
    writeln(e(X2)).

% Run all tests
run_all_phase3_tests :-
    writeln('=== PHASE 3 ARITHMETIC FUNCTIONS TESTS ==='),
    catch(test_trig_functions, E1, writeln(error_trig(E1))),
    catch(test_basic_functions, E2, writeln(error_basic(E2))),
    catch(test_log_functions, E3, writeln(error_log(E3))),
    catch(test_binary_functions, E4, writeln(error_binary(E4))),
    catch(test_iso_functions, E5, writeln(error_iso(E5))),
    catch(test_bitwise_functions, E6, writeln(error_bitwise(E6))),
    catch(test_constants, E7, writeln(error_constants(E7))),
    writeln('=== PHASE 3 TESTS COMPLETED ===').