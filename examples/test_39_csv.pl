% Test 39: CSV predicates
% Tests: csv_parse, csv_serialize, csv_read_file, csv_write_file

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
    write('=== Test 39: CSV Predicates ==='), nl, nl,
    test_csv_parse,
    test_csv_serialize,
    test_csv_file_operations,
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
% 1. csv_parse
% ============================================================
test_csv_parse :-
    write('--- csv_parse ---'), nl,
    run_test(parse_single_row,
        (csv_parse('name,age,city', Rows),
         is_list(Rows), length(Rows, 1))),
    run_test(parse_multiple_rows,
        (csv_parse('a,b,c\n1,2,3', Rows),
         length(Rows, 2))),
    run_test(parse_numbers,
        (csv_parse('42,3.14,hello', Rows),
         Rows = [row(42, 3.14, hello)])),
    run_test(parse_empty_lines_skipped,
        (csv_parse('a,b\n\nc,d', Rows),
         length(Rows, 2))),
    run_test(parse_single_column,
        (csv_parse('alpha\nbeta\ngamma', Rows),
         length(Rows, 3))),
    run_test(parse_row_structure,
        (csv_parse('x,y', Rows),
         Rows = [row(x, y)])).

% ============================================================
% 2. csv_serialize
% ============================================================
test_csv_serialize :-
    write('--- csv_serialize ---'), nl,
    run_test(serialize_single_row,
        (csv_serialize([row(a, b, c)], S),
         atom(S))),
    run_test(serialize_multiple_rows,
        (csv_serialize([row(a, b), row(c, d)], S),
         atom(S))),
    run_test(serialize_numbers,
        (csv_serialize([row(1, 2, 3)], S),
         atom(S))),
    run_test(serialize_roundtrip,
        (csv_serialize([row(hello, 42, world)], S),
         csv_parse(S, Rows),
         Rows == [row(hello, 42, world)])),
    run_test(serialize_mixed_types,
        (csv_serialize([row(alice, 30, rome), row(bob, 25, milan)], S),
         atom(S))).

% ============================================================
% 3. File operations
% ============================================================
test_csv_file_operations :-
    write('--- csv file operations ---'), nl,
    run_test(csv_write_file_3_rows,
        csv_write_file('/tmp/jprolog_test_39.csv',
                       [row('Name', 'Age', 'City'),
                        row('Alice', 30, 'Rome'),
                        row('Bob', 25, 'Milan')])),
    run_test(csv_read_file_count,
        (csv_read_file('/tmp/jprolog_test_39.csv', Rows),
         length(Rows, 3))),
    run_test(csv_file_content_matches,
        (csv_read_file('/tmp/jprolog_test_39.csv', Rows),
         Rows = [row('Name', 'Age', 'City'),
                 row('Alice', 30, 'Rome'),
                 row('Bob', 25, 'Milan')])),
    run_test(csv_file_exists_after_write,
        file_exists('/tmp/jprolog_test_39.csv')).

% ============================================================
% Cleanup
% ============================================================
cleanup :-
    catch(delete_file('/tmp/jprolog_test_39.csv'), _, true).

:- run_all_tests.
