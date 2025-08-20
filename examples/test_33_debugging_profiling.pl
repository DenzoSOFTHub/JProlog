% ===================================================================
% TEST 33: Advanced Debugging and Profiling
% ===================================================================
% Tests: trace/0, spy/1, profiling, debugging predicates

% Profiling predicates
profile_test :-
    profile(expensive_operation(1000)).

expensive_operation(0) :- !.
expensive_operation(N) :-
    N > 0,
    dummy_computation,
    N1 is N - 1,
    expensive_operation(N1).

dummy_computation :-
    X is sin(1.5707963267948966) + cos(0) + exp(1) + log(2.718281828459045).

% Call counting and statistics
call_counter(Predicate) :-
    (   retract(call_count(Predicate, N))
    ->  true
    ;   N = 0
    ),
    N1 is N + 1,
    assert(call_count(Predicate, N1)).

traced_predicate(X) :-
    call_counter(traced_predicate/1),
    format('traced_predicate called with: ~w~n', [X]),
    X > 0.

% Execution tracing
trace_execution(Goal) :-
    format('TRACE: Entering ~w~n', [Goal]),
    (   call(Goal)
    ->  format('TRACE: ~w succeeded~n', [Goal])
    ;   format('TRACE: ~w failed~n', [Goal]),
        fail
    ).

% Performance measurement
time_execution(Goal, Time) :-
    get_time(Start),
    call(Goal),
    get_time(End),
    Time is End - Start.

benchmark_predicate(Predicate, Times, Results) :-
    benchmark_loop(Predicate, Times, [], Results).

benchmark_loop(_, 0, Acc, Acc).
benchmark_loop(Predicate, N, Acc, Results) :-
    N > 0,
    time_execution(Predicate, Time),
    N1 is N - 1,
    benchmark_loop(Predicate, N1, [Time|Acc], Results).

% Memory usage tracking
memory_test(Goal) :-
    garbage_collect,
    statistics(globalused, Before),
    call(Goal),
    garbage_collect,
    statistics(globalused, After),
    Used is After - Before,
    format('Memory used: ~w bytes~n', [Used]).

% Stack depth monitoring
stack_depth_test(N) :-
    stack_depth_test_impl(N, 0).

stack_depth_test_impl(0, Depth) :-
    format('Maximum depth reached: ~w~n', [Depth]).
stack_depth_test_impl(N, Depth) :-
    N > 0,
    N1 is N - 1,
    Depth1 is Depth + 1,
    stack_depth_test_impl(N1, Depth1).

% Debug point system
debug_point(Label, Goal) :-
    (   debugging(Label)
    ->  format('DEBUG [~w]: ~w~n', [Label, Goal]),
        call(Goal)
    ;   call(Goal)
    ).

set_debug(Label) :-
    assert(debugging(Label)).

clear_debug(Label) :-
    retractall(debugging(Label)).

% Assertion checking
assert_check(Condition, Message) :-
    (   call(Condition)
    ->  true
    ;   format('ASSERTION FAILED: ~w~n', [Message]),
        throw(assertion_failed(Message))
    ).

test_with_assertions :-
    X = 5,
    assert_check(X > 0, 'X should be positive'),
    assert_check(X < 10, 'X should be less than 10'),
    format('All assertions passed~n').

% Performance regression testing
regression_test(TestName, Goal, ExpectedTime) :-
    time_execution(Goal, ActualTime),
    (   ActualTime =< ExpectedTime
    ->  format('PASS: ~w completed in ~2f seconds~n', [TestName, ActualTime])
    ;   format('FAIL: ~w took ~2f seconds (expected <= ~2f)~n', 
              [TestName, ActualTime, ExpectedTime])
    ).

% Call graph analysis
:- dynamic call_graph/2.

trace_calls(Goal) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, goal, ParentGoal),
    assert(call_graph(ParentGoal, Goal)),
    call(Goal).

% Test queries:
% ?- profile_test.
% ?- traced_predicate(5).
% ?- trace_execution(member(2, [1,2,3])).
% ?- time_execution(length([1,2,3,4,5], _), Time).
% ?- benchmark_predicate(dummy_computation, 10, Results).
% ?- memory_test(findall(X, between(1, 1000, X), _)).
% ?- stack_depth_test(100).
% ?- set_debug(test), debug_point(test, write('Hello')).
% ?- test_with_assertions.
% ?- regression_test('list_length', length([1,2,3,4,5], _), 0.001).