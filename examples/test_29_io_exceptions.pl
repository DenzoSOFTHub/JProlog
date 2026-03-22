% Test 29: I/O and Exception Handling Tests
% ISS-2025-0171 - I/O and exception fixes

% === Test 1: catch/3 basic ===
test_catch_basic :-
    catch(
        throw(my_error),
        my_error,
        true
    ).

% === Test 2: catch/3 with ISO error ===
test_catch_type_error :-
    catch(
        (X is foo + 1, fail),
        error(type_error(evaluable, _), _),
        true
    ).

% === Test 3: catch/3 recovery goal succeeds ===
test_catch_recovery :-
    catch(
        throw(oops),
        oops,
        X = recovered
    ),
    X == recovered.

% === Test 4: Nested catch ===
test_nested_catch :-
    catch(
        catch(
            throw(inner),
            outer,
            fail
        ),
        inner,
        true
    ).

% === Test 5: catch/3 recovery throws (should propagate) ===
test_catch_recovery_throws :-
    catch(
        catch(
            throw(first),
            first,
            throw(second)
        ),
        second,
        true
    ).

% === Test 6: Division by zero error ===
test_div_zero :-
    catch(
        (X is 1 / 0, fail),
        error(evaluation_error(zero_divisor), _),
        true
    ).

% === Test 7: Unbound variable in arithmetic ===
test_unbound_arith :-
    catch(
        (X is Y + 1, fail),
        _,
        true
    ).

% === Test 8: Exception preserves bindings before throw ===
test_exception_bindings :-
    X = hello,
    catch(
        (Y = world, throw(test)),
        test,
        true
    ),
    X == hello.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== I/O & Exception Tests (Phase 10) ==='), nl.
:- run_test('catch/3 basic', test_catch_basic).
:- run_test('catch/3 type_error', test_catch_type_error).
:- run_test('catch/3 recovery succeeds', test_catch_recovery).
:- run_test('nested catch', test_nested_catch).
:- run_test('catch recovery throws', test_catch_recovery_throws).
:- run_test('division by zero', test_div_zero).
:- run_test('unbound variable in arithmetic', test_unbound_arith).
:- run_test('exception preserves bindings', test_exception_bindings).
:- write('=== I/O & Exception Tests Complete ==='), nl.
