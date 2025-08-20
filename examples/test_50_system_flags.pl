% test_50_system_flags.pl
% Comprehensive test for ISO Prolog system flags (current_prolog_flag/2, set_prolog_flag/2)
% Tests Phase 7 implementation

% Test 1: Query basic system information flags
test_system_info :-
    current_prolog_flag(version, Version),
    write('JProlog version: '), writeln(Version),
    current_prolog_flag(dialect, Dialect), 
    write('Dialect: '), writeln(Dialect),
    current_prolog_flag(bounded, Bounded),
    write('Integers bounded: '), writeln(Bounded).

% Test 2: Query numeric system limits
test_system_limits :-
    current_prolog_flag(max_integer, MaxInt),
    write('Max integer: '), writeln(MaxInt),
    current_prolog_flag(min_integer, MinInt),
    write('Min integer: '), writeln(MinInt),
    current_prolog_flag(max_arity, MaxArity),
    write('Max arity: '), writeln(MaxArity).

% Test 3: Query runtime behavior flags
test_runtime_flags :-
    current_prolog_flag(debug, Debug),
    write('Debug mode: '), writeln(Debug),
    current_prolog_flag(unknown, Unknown),
    write('Unknown predicate handling: '), writeln(Unknown),
    current_prolog_flag(occurs_check, OccursCheck),
    write('Occurs check: '), writeln(OccursCheck).

% Test 4: Query character and string handling flags
test_character_flags :-
    current_prolog_flag(character_escapes, CharEscapes),
    write('Character escapes: '), writeln(CharEscapes),
    current_prolog_flag(double_quotes, DoubleQuotes),
    write('Double quotes: '), writeln(DoubleQuotes),
    current_prolog_flag(char_conversion, CharConv),
    write('Char conversion: '), writeln(CharConv).

% Test 5: Set modifiable flags
test_set_flags :-
    % Test setting debug flag
    write('Setting debug to on...'), nl,
    set_prolog_flag(debug, on),
    current_prolog_flag(debug, Debug1),
    write('Debug now: '), writeln(Debug1),
    
    % Test setting debug back to off
    set_prolog_flag(debug, off),
    current_prolog_flag(debug, Debug2),
    write('Debug back to: '), writeln(Debug2),
    
    % Test setting unknown flag
    write('Setting unknown to fail...'), nl,
    set_prolog_flag(unknown, fail),
    current_prolog_flag(unknown, Unknown1),
    write('Unknown now: '), writeln(Unknown1),
    
    % Reset unknown back to error
    set_prolog_flag(unknown, error),
    current_prolog_flag(unknown, Unknown2),
    write('Unknown back to: '), writeln(Unknown2).

% Test 6: Try to set read-only flags (should fail gracefully)
test_readonly_flags :-
    write('Testing read-only flag protection...'), nl,
    catch(
        set_prolog_flag(version, '999.999'),
        Error,
        (write('Cannot set version (expected): '), writeln(Error))
    ),
    catch(
        set_prolog_flag(bounded, false),
        Error2, 
        (write('Cannot set bounded (expected): '), writeln(Error2))
    ).

% Test 7: Test invalid flag values
test_invalid_values :-
    write('Testing invalid flag value protection...'), nl,
    catch(
        set_prolog_flag(debug, invalid_value),
        Error1,
        (write('Invalid debug value rejected (expected): '), writeln(Error1))
    ),
    catch(
        set_prolog_flag(unknown, invalid_mode),
        Error2,
        (write('Invalid unknown value rejected (expected): '), writeln(Error2))
    ).

% Test 8: Enumerate all flags
test_enumerate_flags :-
    write('All system flags:'), nl,
    forall(
        current_prolog_flag(Flag, Value),
        (write('  '), write(Flag), write(' = '), writeln(Value))
    ).

% Test 9: Test specific ISO-required flags
test_iso_flags :-
    write('Testing ISO-required flags...'), nl,
    % Test character_escapes flag
    current_prolog_flag(character_escapes, CharEsc),
    write('character_escapes: '), writeln(CharEsc),
    
    % Test strict_iso flag
    current_prolog_flag(strict_iso, StrictISO),
    write('strict_iso: '), writeln(StrictISO),
    
    % Test integer_rounding_function
    current_prolog_flag(integer_rounding_function, Rounding),
    write('integer_rounding_function: '), writeln(Rounding).

% Test 10: Test stack_limit numeric flag
test_numeric_flags :-
    write('Testing numeric flags...'), nl,
    current_prolog_flag(stack_limit, StackLimit),
    write('Current stack_limit: '), writeln(StackLimit),
    
    % Try to set stack limit
    write('Setting stack_limit to 2000000...'), nl,
    set_prolog_flag(stack_limit, 2000000),
    current_prolog_flag(stack_limit, NewLimit),
    write('New stack_limit: '), writeln(NewLimit),
    
    % Reset stack limit
    set_prolog_flag(stack_limit, 1000000),
    write('Reset stack_limit back to 1000000'), nl.

% Main test runner
run_all_tests :-
    writeln('=== JProlog System Flags Test Suite ==='),
    nl,
    
    writeln('Test 1: System Information Flags'),
    test_system_info, nl,
    
    writeln('Test 2: System Limits'),
    test_system_limits, nl,
    
    writeln('Test 3: Runtime Flags'),
    test_runtime_flags, nl,
    
    writeln('Test 4: Character Handling Flags'),
    test_character_flags, nl,
    
    writeln('Test 5: Setting Modifiable Flags'),
    test_set_flags, nl,
    
    writeln('Test 6: Read-Only Flag Protection'),
    test_readonly_flags, nl,
    
    writeln('Test 7: Invalid Value Protection'),
    test_invalid_values, nl,
    
    writeln('Test 8: Flag Enumeration'),
    test_enumerate_flags, nl,
    
    writeln('Test 9: ISO-Required Flags'),
    test_iso_flags, nl,
    
    writeln('Test 10: Numeric Flags'),
    test_numeric_flags, nl,
    
    writeln('=== All System Flags Tests Completed Successfully ===').

% Simple individual test for quick verification
simple_test :-
    current_prolog_flag(version, V),
    write('Version: '), writeln(V),
    current_prolog_flag(dialect, D),
    write('Dialect: '), writeln(D).