% Test 35: OS/System predicates
% Tests: system_time, os_name, cpu_count, free_memory, total_memory,
%        pid, getenv, shell_output

% ============================================================
% Test framework
% ============================================================
:- dynamic(test_passed/1).
:- dynamic(test_failed/2).
:- dynamic(test_count/1).
:- assert(test_count(0)).

run_test(Name, Goal) :-
    retract(test_count(N)), N1 is N + 1, assert(test_count(N1)),
    copy_term(Goal, GoalCopy),
    ( catch(call(GoalCopy), E, (assert(test_failed(Name, E)), fail))
    -> assert(test_passed(Name)),
       write('  PASS: '), write(Name), nl
    ;  ( \+ test_failed(Name, _) -> assert(test_failed(Name, failed)) ; true ),
       write('  FAIL: '), write(Name), nl
    ).

run_all_tests :-
    write('=== Test 35: OS/System Predicates ==='), nl, nl,
    test_system_time,
    test_os_name,
    test_cpu_count,
    test_memory,
    test_pid,
    test_getenv,
    test_shell_output,
    nl, write('--- Results ---'), nl,
    aggregate_all(count, test_passed(_), Passed),
    aggregate_all(count, test_failed(_, _), Failed),
    test_count(Total),
    write('Passed: '), write(Passed), write('/'), write(Total), nl,
    write('Failed: '), write(Failed), nl,
    ( Failed > 0
    -> forall(test_failed(N, R), (write('  '), write(N), write(': '), write(R), nl))
    ; true
    ).

% ============================================================
% 1. system_time
% ============================================================
test_system_time :-
    write('--- system_time ---'), nl,
    run_test(system_time_positive,
        (system_time(T), T > 0)),
    run_test(system_time_is_number,
        (system_time(T), number(T))),
    run_test(system_time_increasing,
        (system_time(T1), system_time(T2), T2 >= T1)).

% ============================================================
% 2. os_name
% ============================================================
test_os_name :-
    write('--- os_name ---'), nl,
    run_test(os_name_returns_atom,
        (os_name(N), atom(N))),
    run_test(os_name_not_empty,
        (os_name(N), atom_length(N, L), L > 0)).

% ============================================================
% 3. cpu_count
% ============================================================
test_cpu_count :-
    write('--- cpu_count ---'), nl,
    run_test(cpu_count_positive,
        (cpu_count(N), N > 0)),
    run_test(cpu_count_is_number,
        (cpu_count(N), number(N))).

% ============================================================
% 4. Memory
% ============================================================
test_memory :-
    write('--- memory ---'), nl,
    run_test(free_memory_positive,
        (free_memory(M), M > 0)),
    run_test(total_memory_positive,
        (total_memory(M), M > 0)),
    run_test(free_leq_total,
        (free_memory(F), total_memory(T), F =< T)).

% ============================================================
% 5. PID
% ============================================================
test_pid :-
    write('--- pid ---'), nl,
    run_test(pid_positive,
        (pid(P), P > 0)),
    run_test(pid_is_number,
        (pid(P), number(P))),
    run_test(pid_consistent,
        (pid(P1), pid(P2), P1 == P2)).

% ============================================================
% 6. getenv
% ============================================================
test_getenv :-
    write('--- getenv ---'), nl,
    run_test(getenv_path_exists,
        (getenv('PATH', V), atom(V))),
    run_test(getenv_nonexistent_fails,
        \+ getenv('JPROLOG_NONEXISTENT_VAR_XYZ', _)).

% ============================================================
% 7. shell_output
% ============================================================
test_shell_output :-
    write('--- shell_output ---'), nl,
    run_test(shell_output_echo,
        (shell_output('echo hello', Output, Code),
         Code == 0, atom(Output))),
    run_test(shell_output_exit_0,
        (shell_output('exit 0', _, Code), Code == 0)),
    run_test(shell_output_exit_1,
        (shell_output('exit 1', _, Code), Code == 1)).

:- run_all_tests.
