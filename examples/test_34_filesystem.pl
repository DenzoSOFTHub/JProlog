% Test 34: File system predicates
% Tests: file_exists, directory_exists, make_directory, delete_file,
%        rename_file, copy_file, file_size, directory_files,
%        absolute_file_name, read_file_to_atom, write_atom_to_file

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
    write('=== Test 34: File System Predicates ==='), nl, nl,
    test_file_exists,
    test_directory_exists,
    test_write_read_file,
    test_file_size,
    test_copy_file,
    test_rename_file,
    test_delete_file,
    test_make_directory,
    test_absolute_file_name,
    test_directory_files,
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
% 1. file_exists
% ============================================================
test_file_exists :-
    write('--- file_exists ---'), nl,
    run_test(file_exists_pom,
        file_exists('pom.xml')),
    run_test(file_not_exists,
        \+ file_exists('nonexistent_file_xyz_34.txt')).

% ============================================================
% 2. directory_exists
% ============================================================
test_directory_exists :-
    write('--- directory_exists ---'), nl,
    run_test(dir_exists_src,
        directory_exists(src)),
    run_test(dir_not_exists,
        \+ directory_exists('nonexistent_dir_xyz_34')).

% ============================================================
% 3. write and read file
% ============================================================
test_write_read_file :-
    write('--- write/read file ---'), nl,
    run_test(write_atom_to_file,
        write_atom_to_file('/tmp/jprolog_test_34.txt', 'Hello JProlog!')),
    run_test(read_file_to_atom,
        (read_file_to_atom('/tmp/jprolog_test_34.txt', Content),
         Content == 'Hello JProlog!')),
    run_test(file_exists_after_write,
        file_exists('/tmp/jprolog_test_34.txt')).

% ============================================================
% 4. file_size
% ============================================================
test_file_size :-
    write('--- file_size ---'), nl,
    run_test(file_size_positive,
        (file_size('/tmp/jprolog_test_34.txt', S), S > 0)),
    run_test(file_size_correct_bytes,
        (file_size('/tmp/jprolog_test_34.txt', S), S == 14)).

% ============================================================
% 5. copy_file
% ============================================================
test_copy_file :-
    write('--- copy_file ---'), nl,
    run_test(copy_file_succeeds,
        (copy_file('/tmp/jprolog_test_34.txt', '/tmp/jprolog_test_34_copy.txt'),
         file_exists('/tmp/jprolog_test_34_copy.txt'))),
    run_test(copy_preserves_content,
        (read_file_to_atom('/tmp/jprolog_test_34_copy.txt', C),
         C == 'Hello JProlog!')).

% ============================================================
% 6. rename_file
% ============================================================
test_rename_file :-
    write('--- rename_file ---'), nl,
    run_test(rename_file_succeeds,
        (rename_file('/tmp/jprolog_test_34_copy.txt', '/tmp/jprolog_test_34_renamed.txt'),
         file_exists('/tmp/jprolog_test_34_renamed.txt'))),
    run_test(rename_old_gone,
        \+ file_exists('/tmp/jprolog_test_34_copy.txt')),
    run_test(rename_new_has_content,
        (read_file_to_atom('/tmp/jprolog_test_34_renamed.txt', C),
         C == 'Hello JProlog!')).

% ============================================================
% 7. delete_file
% ============================================================
test_delete_file :-
    write('--- delete_file ---'), nl,
    run_test(delete_file_succeeds,
        (delete_file('/tmp/jprolog_test_34_renamed.txt'),
         \+ file_exists('/tmp/jprolog_test_34_renamed.txt'))).

% ============================================================
% 8. make_directory
% ============================================================
test_make_directory :-
    write('--- make_directory ---'), nl,
    run_test(make_directory_basic,
        (make_directory('/tmp/jprolog_test_dir_34'),
         directory_exists('/tmp/jprolog_test_dir_34'))),
    run_test(make_directory_path_nested,
        (make_directory_path('/tmp/jprolog_test_dir_34/sub/deep'),
         directory_exists('/tmp/jprolog_test_dir_34/sub/deep'))).

% ============================================================
% 9. absolute_file_name
% ============================================================
test_absolute_file_name :-
    write('--- absolute_file_name ---'), nl,
    run_test(absolute_file_name_relative,
        (absolute_file_name('pom.xml', Abs), atom(Abs))),
    run_test(absolute_starts_with_slash,
        (absolute_file_name('pom.xml', Abs),
         atom_codes(Abs, [47|_]))).

% ============================================================
% 10. directory_files
% ============================================================
test_directory_files :-
    write('--- directory_files ---'), nl,
    run_test(directory_files_is_list,
        (directory_files(src, Files), is_list(Files))),
    run_test(directory_files_contains_main,
        (directory_files(src, Files), member(main, Files))).

% ============================================================
% Cleanup
% ============================================================
cleanup :-
    catch(delete_file('/tmp/jprolog_test_34.txt'), _, true),
    catch(delete_file('/tmp/jprolog_test_dir_34/sub/deep'), _, true),
    catch(delete_file('/tmp/jprolog_test_dir_34/sub'), _, true),
    catch(delete_file('/tmp/jprolog_test_dir_34'), _, true).

:- run_all_tests.
