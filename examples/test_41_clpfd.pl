% Test 41: CLP(FD) - Constraint Logic Programming over Finite Domains
% Tests: in/2, #=/2, #\=/2, #</2, #>/2, #=</2, #>=/2,
%        all_different/1, label/1, labeling/2, indomain/1, fd_dom/2, fd_size/2

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
    write('=== Test 41: CLP(FD) Predicates ==='), nl, nl,
    test_domain,
    test_constraints,
    test_label,
    test_all_different,
    test_arithmetic,
    test_introspection,
    test_puzzles,
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
% 1. Domain specification (in/2)
% ============================================================
test_domain :-
    write('--- Domain (in/2) ---'), nl,
    run_test(in_range_basic,
        (in(X, '..'(1, 5)), label([X]), X >= 1, X =< 5)),
    run_test(in_range_label_all,
        (findall(V, (in(V, '..'(1, 3)), label([V])), Vs), Vs == [1, 2, 3])),
    run_test(in_ground_check_pass,
        (X = 3, in(X, '..'(1, 5)))),
    run_test(in_ground_check_fail,
        (\+ (X = 7, in(X, '..'(1, 5))))).

% ============================================================
% 2. Constraint predicates
% ============================================================
test_constraints :-
    write('--- Constraints ---'), nl,
    run_test(eq_ground,
        ('#='(3, 3))),
    run_test(eq_ground_fail,
        (\+ '#='(3, 4))),
    run_test(neq_ground,
        ('#\\='(3, 4))),
    run_test(neq_ground_fail,
        (\+ '#\\='(3, 3))),
    run_test(lt_ground,
        ('#<'(2, 5))),
    run_test(lt_ground_fail,
        (\+ '#<'(5, 2))),
    run_test(gt_ground,
        ('#>'(5, 2))),
    run_test(leq_ground,
        ('#=<'(3, 3))),
    run_test(geq_ground,
        ('#>='(5, 3))),
    run_test(neq_var_filters,
        (in(X, '..'(1, 3)), in(Y, '..'(1, 3)), '#\\='(X, Y),
         findall(A-B, label([X, Y]), Sols), length(Sols, 6))).

% ============================================================
% 3. Labeling
% ============================================================
test_label :-
    write('--- Labeling ---'), nl,
    run_test(label_single_var,
        (in(X, '..'(1, 4)), findall(X, label([X]), Vs), length(Vs, 4))),
    run_test(label_two_vars,
        (in(X, '..'(1, 2)), in(Y, '..'(1, 2)),
         findall(X-Y, label([X, Y]), Sols), length(Sols, 4))),
    run_test(label_with_constraint,
        (in(X, '..'(1, 3)), in(Y, '..'(1, 3)), '#<'(X, Y),
         findall(X-Y, label([X, Y]), Sols), length(Sols, 3))),
    run_test(labeling_ff,
        (in(X, '..'(1, 2)), in(Y, '..'(1, 3)),
         findall(X-Y, labeling([ff], [X, Y]), Sols), length(Sols, 6))),
    run_test(indomain_basic,
        (in(X, '..'(1, 3)), findall(X, indomain(X), Vs), Vs == [1, 2, 3])).

% ============================================================
% 4. all_different/1
% ============================================================
test_all_different :-
    write('--- all_different ---'), nl,
    run_test(all_diff_3_vars,
        (in(X, '..'(1, 3)), in(Y, '..'(1, 3)), in(Z, '..'(1, 3)),
         all_different([X, Y, Z]),
         findall(X-Y-Z, label([X, Y, Z]), Sols), length(Sols, 6))),
    run_test(all_diff_ground_pass,
        all_different([1, 2, 3])),
    run_test(all_diff_ground_fail,
        (\+ all_different([1, 2, 1]))).

% ============================================================
% 5. Arithmetic constraints
% ============================================================
test_arithmetic :-
    write('--- Arithmetic Constraints ---'), nl,
    run_test(eq_var_const,
        (in(X, '..'(1, 10)), '#='(X, 5), label([X]), X == 5)),
    run_test(lt_var_const,
        (in(X, '..'(1, 10)), '#<'(X, 4),
         findall(X, label([X]), Vs), Vs == [1, 2, 3])),
    run_test(gt_var_const,
        (in(X, '..'(1, 5)), '#>'(X, 3),
         findall(X, label([X]), Vs), Vs == [4, 5])),
    run_test(eq_arithmetic_add,
        (in(X, '..'(1, 10)), in(Y, '..'(1, 10)), '#='(X, '+'(Y, 3)),
         Y = 2, label([X]), X == 5)).

% ============================================================
% 6. Domain introspection
% ============================================================
test_introspection :-
    write('--- Introspection ---'), nl,
    run_test(fd_size_basic,
        (in(X, '..'(1, 10)), fd_size(X, S), S == 10)),
    run_test(fd_size_after_constraint,
        (in(X, '..'(1, 10)), '#<'(X, 5), fd_size(X, S), S == 4)),
    % ISS-2025-0793 (4.6 wave Q7): since 4.5.0 fd_dom/2 answers the SWI domain
    % term (From..To, joined with \/), not a list of values.
    run_test(fd_dom_basic,
        (in(X, '..'(1, 3)), fd_dom(X, Dom), Dom == '..'(1, 3))),
    run_test(fd_dom_holes,
        (in(X, '..'(1, 5)), '#\\='(X, 3), fd_dom(X, Dom),
         Dom == '\\/'('..'(1, 2), '..'(4, 5)))).

% ============================================================
% 7. Puzzle examples
% ============================================================
test_puzzles :-
    write('--- Puzzles ---'), nl,
    run_test(send_more_money_feasible,
        % Simple version: find X,Y,Z where X+Y=Z, all different, in 1..9
        (in(X, '..'(1, 9)), in(Y, '..'(1, 9)), in(Z, '..'(1, 9)),
         all_different([X, Y, Z]), '#='(Z, '+'(X, Y)),
         label([X, Y, Z]), Z =:= X + Y)),
    run_test(n_queens_4_has_solutions,
        % 4-queens: each queen on different row/column/diagonal
        (in(Q1, '..'(1, 4)), in(Q2, '..'(1, 4)),
         in(Q3, '..'(1, 4)), in(Q4, '..'(1, 4)),
         all_different([Q1, Q2, Q3, Q4]),
         % Diagonal constraints
         '#\\='('-'(Q1, Q2), 1), '#\\='('-'(Q2, Q1), 1),
         '#\\='('-'(Q1, Q3), 2), '#\\='('-'(Q3, Q1), 2),
         '#\\='('-'(Q1, Q4), 3), '#\\='('-'(Q4, Q1), 3),
         '#\\='('-'(Q2, Q3), 1), '#\\='('-'(Q3, Q2), 1),
         '#\\='('-'(Q2, Q4), 2), '#\\='('-'(Q4, Q2), 2),
         '#\\='('-'(Q3, Q4), 1), '#\\='('-'(Q4, Q3), 1),
         label([Q1, Q2, Q3, Q4]))).

:- run_all_tests.
