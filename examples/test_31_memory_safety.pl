% Test 31: Memory Safety and Resource Management Tests
% ISS-2025-0173 - Memory leak fixes

% === Test 1: JavaFFI reference management ===
test_ffi_create_release :-
    java_new('java.lang.String', ['test'], Ref),
    java_release_ref(Ref).

test_ffi_gc :-
    java_new('java.lang.String', ['a'], _),
    java_new('java.lang.String', ['b'], _),
    java_gc.

% === Test 2: Tabling works after many queries ===
:- table memo_fib/2.
memo_fib(0, 0) :- !.
memo_fib(1, 1) :- !.
memo_fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2,
    memo_fib(N1, F1), memo_fib(N2, F2), F is F1 + F2.

test_tabling_repeated :-
    memo_fib(15, 610),
    abolish_all_tables,
    memo_fib(15, 610).

% === Test 3: Multiple assert/retract cycles don't leak ===
test_assert_retract_cycles :-
    between(1, 100, _),
    assert(temp_leak_test(hello)),
    retract(temp_leak_test(hello)),
    fail ; true.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Memory Safety Tests ==='), nl.
:- run_test('FFI create and release ref', test_ffi_create_release).
:- run_test('FFI garbage collect', test_ffi_gc).
:- run_test('Tabling repeated with abolish', test_tabling_repeated).
:- run_test('Assert/retract cycles', test_assert_retract_cycles).
:- write('=== Memory Safety Tests Complete ==='), nl.
