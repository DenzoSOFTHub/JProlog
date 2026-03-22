% Test 40: Logging predicates
% Tests: log_info, log_warning, log_error, log_debug, log_level, log_to_file

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
    write('=== Test 40: Logging Predicates ==='), nl, nl,
    test_log_levels,
    test_log_messages,
    test_log_to_file,
    cleanup,
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
% 1. Log levels
% ============================================================
test_log_levels :-
    write('--- log levels ---'), nl,
    run_test(set_level_info,
        log_level(info)),
    run_test(set_level_debug,
        log_level(debug)),
    run_test(set_level_warning,
        log_level(warning)),
    run_test(set_level_error,
        log_level(error)),
    run_test(set_level_all,
        log_level(all)),
    run_test(set_level_off,
        log_level(off)),
    run_test(reset_to_all,
        log_level(all)).

% ============================================================
% 2. Log messages
% ============================================================
test_log_messages :-
    write('--- log messages ---'), nl,
    run_test(log_info_succeeds,
        log_info('This is an info message')),
    run_test(log_warning_succeeds,
        log_warning('This is a warning message')),
    run_test(log_error_succeeds,
        log_error('This is an error message')),
    run_test(log_debug_succeeds,
        log_debug('This is a debug message')).

% ============================================================
% 3. Log to file
% ============================================================
test_log_to_file :-
    write('--- log to file ---'), nl,
    run_test(log_to_file_creates,
        (log_to_file('/tmp/jprolog_test_40.log'),
         log_info('test message to file'),
         file_exists('/tmp/jprolog_test_40.log'))),
    run_test(log_file_has_content,
        (file_size('/tmp/jprolog_test_40.log', S), S > 0)),
    run_test(log_multiple_to_file,
        (log_info('message one'),
         log_warning('message two'),
         log_error('message three'),
         file_size('/tmp/jprolog_test_40.log', S), S > 0)).

% ============================================================
% Cleanup
% ============================================================
cleanup :-
    log_level(info),
    catch(delete_file('/tmp/jprolog_test_40.log'), _, true),
    catch(delete_file('/tmp/jprolog_test_40.log.lck'), _, true).

:- run_all_tests.
