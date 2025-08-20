% ===================================================================
% TEST 43: Complex Control Structures and Error Handling
% ===================================================================
% Advanced examples using ->, ;, , and error handling patterns

% Example 1: Safe Division with Error Handling
safe_divide(X, Y, Result) :-
    (   Y =:= 0 ->
        Result = error(division_by_zero)
    ;   Result is X / Y
    ).

% Example 2: Type-Safe Operations with If-Then-Else
type_safe_operation(X, Y, Operation, Result) :-
    (   (number(X), number(Y)) ->
        (   Operation = add ->
            Result is X + Y
        ;   Operation = multiply ->
            Result is X * Y
        ;   Operation = subtract ->
            Result is X - Y
        ;   Result = error(unknown_operation)
        )
    ;   Result = error(invalid_types)
    ).

% Example 3: List Processing with Error Handling
safe_head([], error(empty_list)).
safe_head([H|_], H).

safe_tail([], error(empty_list)).
safe_tail([_|T], T).

safe_nth(N, List, Result) :-
    (   (integer(N), N >= 0) ->
        (   length(List, Len) ->
            (   N < Len ->
                nth0(N, List, Result)
            ;   Result = error(index_out_of_bounds)
            )
        ;   Result = error(invalid_list)
        )
    ;   Result = error(invalid_index)
    ).

% Example 4: Database Operations with Error Handling  
safe_assert(Term, Result) :-
    (   callable(Term) ->
        (   assert(Term),
            Result = success(asserted)
        )
    ;   Result = error(not_callable)
    ).

safe_retract(Term, Result) :-
    (   callable(Term) ->
        (   retract(Term) ->
            Result = success(retracted)
        ;   Result = error(not_found)
        )
    ;   Result = error(not_callable)
    ).

% Example 5: Complex Condition Chains
classify_number(N, Classification) :-
    (   number(N) ->
        (   N > 0 ->
            (   N =:= 1 ->
                Classification = one
            ;   N < 10 ->
                Classification = small_positive
            ;   N < 100 ->
                Classification = medium_positive
            ;   Classification = large_positive
            )
        ;   N =:= 0 ->
            Classification = zero
        ;   N > -10 ->
            Classification = small_negative
        ;   Classification = large_negative
        )
    ;   Classification = error(not_a_number)
    ).

% Example 6: Pattern Matching with Multiple Options
process_term(Term, Result) :-
    (   var(Term) ->
        Result = variable
    ;   atom(Term) ->
        (   Term = true ->
            Result = boolean_true
        ;   Term = false ->
            Result = boolean_false
        ;   Result = regular_atom
        )
    ;   number(Term) ->
        (   integer(Term) ->
            Result = integer_number
        ;   Result = float_number
        )
    ;   compound(Term) ->
        (   functor(Term, f, 2) ->
            Result = binary_functor_f
        ;   functor(Term, g, 1) ->
            Result = unary_functor_g
        ;   Result = other_compound
        )
    ;   Result = unknown_type
    ).

% Example 7: List Validation with Error Accumulation
validate_number_list([], []).
validate_number_list([H|T], [Error|RestErrors]) :-
    \+ number(H), !,
    Error = error(not_number, H),
    validate_number_list(T, RestErrors).
validate_number_list([H|T], Errors) :-
    number(H),
    validate_number_list(T, Errors).

validate_list_completely(List, Result) :-
    validate_number_list(List, Errors),
    (   Errors = [] ->
        Result = valid(all_numbers)
    ;   Result = invalid(Errors)
    ).

% Example 8: Nested Control Structures
complex_decision(Input, Output) :-
    (   atom(Input) ->
        (   Input = start ->
            Output = initialized
        ;   Input = stop ->
            Output = terminated
        ;   Input = pause ->
            Output = suspended
        ;   Output = unknown_command
        )
    ;   number(Input) ->
        (   Input > 0 ->
            (   Input > 100 ->
                Output = high_value
            ;   Output = positive_value
            )
        ;   Input =:= 0 ->
            Output = zero_value
        ;   Output = negative_value
        )
    ;   compound(Input) ->
        (   functor(Input, command, Arity) ->
            (   Arity =:= 1 ->
                arg(1, Input, Arg),
                (   atom(Arg) ->
                    Output = single_atom_command
                ;   Output = single_non_atom_command
                )
            ;   Output = multi_arg_command
            )
        ;   Output = non_command_compound
        )
    ;   Output = unrecognized_input
    ).

% Example 9: Error Recovery Patterns
try_operations(Input, Result) :-
    (   try_operation_a(Input, TempResult) ->
        Result = success(operation_a, TempResult)
    ;   try_operation_b(Input, TempResult) ->
        Result = success(operation_b, TempResult)
    ;   try_operation_c(Input, TempResult) ->
        Result = success(operation_c, TempResult)
    ;   Result = failure(all_operations_failed)
    ).

try_operation_a(X, Result) :-
    number(X),
    X > 0,
    Result is X * 2.

try_operation_b(X, Result) :-
    atom(X),
    atom_length(X, Result).

try_operation_c(X, Result) :-
    compound(X),
    functor(X, _, Result).

% Example 10: State Machine with Error Handling
state_transition(CurrentState, Event, NewState, Action) :-
    (   CurrentState = idle ->
        (   Event = start ->
            NewState = running,
            Action = initialize_system
        ;   Event = configure ->
            NewState = configuring,
            Action = enter_config_mode
        ;   NewState = CurrentState,
            Action = error(invalid_event_in_idle)
        )
    ;   CurrentState = running ->
        (   Event = stop ->
            NewState = idle,
            Action = shutdown_system
        ;   Event = pause ->
            NewState = paused,
            Action = suspend_operations
        ;   Event = error ->
            NewState = error_state,
            Action = handle_error
        ;   NewState = CurrentState,
            Action = error(invalid_event_in_running)
        )
    ;   CurrentState = paused ->
        (   Event = resume ->
            NewState = running,
            Action = resume_operations
        ;   Event = stop ->
            NewState = idle,
            Action = shutdown_from_pause
        ;   NewState = CurrentState,
            Action = error(invalid_event_in_paused)
        )
    ;   CurrentState = configuring ->
        (   Event = save ->
            NewState = idle,
            Action = save_configuration
        ;   Event = cancel ->
            NewState = idle,
            Action = discard_changes
        ;   NewState = CurrentState,
            Action = error(invalid_event_in_configuring)
        )
    ;   CurrentState = error_state ->
        (   Event = reset ->
            NewState = idle,
            Action = system_reset
        ;   NewState = CurrentState,
            Action = error(system_in_error_state)
        )
    ;   NewState = CurrentState,
        Action = error(unknown_state)
    ).

% Testing predicates for each example
test_safe_divide :-
    safe_divide(10, 2, R1),
    safe_divide(10, 0, R2),
    write('Safe divide: '), write(R1), write(', '), write(R2), nl.

test_type_safe :-
    type_safe_operation(5, 3, add, R1),
    type_safe_operation(hello, 3, add, R2),
    write('Type safe: '), write(R1), write(', '), write(R2), nl.

test_safe_list :-
    safe_head([a,b,c], R1),
    safe_head([], R2),
    write('Safe head: '), write(R1), write(', '), write(R2), nl.

test_classify :-
    classify_number(5, R1),
    classify_number(-15, R2),
    classify_number(hello, R3),
    write('Classify: '), write(R1), write(', '), write(R2), write(', '), write(R3), nl.

test_process_term :-
    process_term(42, R1),
    process_term(true, R2),
    process_term(f(a,b), R3),
    write('Process term: '), write(R1), write(', '), write(R2), write(', '), write(R3), nl.

test_validate_list :-
    validate_list_completely([1,2,3], R1),
    validate_list_completely([1,hello,3], R2),
    write('Validate list: '), write(R1), write(', '), write(R2), nl.

test_complex_decision :-
    complex_decision(start, R1),
    complex_decision(150, R2),
    complex_decision(command(go), R3),
    write('Complex decision: '), write(R1), write(', '), write(R2), write(', '), write(R3), nl.

test_try_operations :-
    try_operations(5, R1),
    try_operations(hello, R2),
    try_operations([a,b], R3),
    write('Try operations: '), write(R1), write(', '), write(R2), write(', '), write(R3), nl.

test_state_machine :-
    state_transition(idle, start, S1, A1),
    state_transition(running, pause, S2, A2),
    state_transition(idle, invalid, S3, A3),
    write('State machine: '), nl,
    write('  '), write(S1), write(' - '), write(A1), nl,
    write('  '), write(S2), write(' - '), write(A2), nl,
    write('  '), write(S3), write(' - '), write(A3), nl.

% Run all control structure tests
run_control_tests :-
    write('=== Control Structure and Error Handling Tests ==='), nl,
    test_safe_divide,
    test_type_safe,
    test_safe_list,
    test_classify,
    test_process_term,
    test_validate_list,
    test_complex_decision,
    test_try_operations,
    test_state_machine,
    write('Control structure tests completed'), nl.