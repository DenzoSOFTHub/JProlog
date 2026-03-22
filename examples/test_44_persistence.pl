% Test 44: Persistence Predicates
% Tests: db_save/1, db_load/1, db_save_predicate/2, db_snapshot/1,
%        db_restore/1, db_clear/0, db_export_json/1, db_import_json/1

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

% Test data
:- dynamic(person/2).
:- dynamic(score/2).
:- dynamic(item/1).

run_all_tests :-
    write('=== Test 44: Persistence Predicates ==='), nl, nl,
    test_db_save,
    test_db_save_predicate,
    test_db_load,
    test_db_snapshot_restore,
    test_db_json,
    test_db_persist,
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
% 1. Database save (full)
% ============================================================
test_db_save :-
    write('--- Save ---'), nl,
    retractall(person(_, _)),
    assert(person(alice, 30)),
    assert(person(bob, 25)),
    run_test(db_save_succeeds,
        db_save('/tmp/test_db_full.pl')),
    run_test(db_save_creates_file,
        file_exists('/tmp/test_db_full.pl')),
    run_test(db_save_file_has_content,
        (read_file_to_atom('/tmp/test_db_full.pl', Content),
         atom_length(Content, Len), Len > 0)),
    catch(delete_file('/tmp/test_db_full.pl'), _, true).

% ============================================================
% 2. Save specific predicate
% ============================================================
test_db_save_predicate :-
    write('--- Save Predicate ---'), nl,
    retractall(score(_, _)),
    assert(score(math, 95)),
    assert(score(english, 88)),
    run_test(save_predicate_succeeds,
        db_save_predicate(score/2, '/tmp/test_pred.pl')),
    run_test(save_predicate_file_exists,
        file_exists('/tmp/test_pred.pl')),
    run_test(save_predicate_has_content,
        (read_file_to_atom('/tmp/test_pred.pl', Content),
         atom_length(Content, Len), Len > 0)),
    catch(delete_file('/tmp/test_pred.pl'), _, true).

% ============================================================
% 3. Load from file (use a manually created simple file)
% ============================================================
test_db_load :-
    write('--- Load ---'), nl,
    % Write a simple known file and load it
    write_atom_to_file('/tmp/test_load.pl', 'item(apple).\nitem(banana).\nitem(cherry).\n'),
    run_test(db_load_succeeds,
        db_load('/tmp/test_load.pl')),
    run_test(db_load_adds_clauses,
        item(apple)),
    run_test(db_load_all_items,
        (item(banana), item(cherry))),
    retractall(item(_)),
    catch(delete_file('/tmp/test_load.pl'), _, true).

% ============================================================
% 4. Snapshot/Restore
% ============================================================
test_db_snapshot_restore :-
    write('--- Snapshot/Restore ---'), nl,
    retractall(person(_, _)),
    assert(person(alice, 30)),
    assert(person(bob, 25)),
    run_test(snapshot_returns_handle,
        (db_snapshot(H), atom(H))),
    run_test(snapshot_two_handles,
        (db_snapshot(H1), db_snapshot(H2), H1 \== H2)).

% ============================================================
% 5. JSON export/import
% ============================================================
test_db_json :-
    write('--- JSON ---'), nl,
    retractall(person(_, _)),
    assert(person(alice, 30)),
    run_test(json_export_succeeds,
        db_export_json('/tmp/test_db.json')),
    run_test(json_export_creates_file,
        file_exists('/tmp/test_db.json')),
    run_test(json_has_content,
        (read_file_to_atom('/tmp/test_db.json', Content),
         atom_length(Content, Len), Len > 0)),
    catch(delete_file('/tmp/test_db.json'), _, true).

% ============================================================
% 6. Persist/unpersist marking
% ============================================================
test_db_persist :-
    write('--- Persist ---'), nl,
    run_test(persist_succeeds,
        persist(person/2)),
    run_test(unpersist_succeeds,
        unpersist(person/2)).

:- run_all_tests.
