% Test 33: Date/time predicates
% Tests: get_time, now, today, date_add, date_diff,
%        day_of_week, date_parts, time_parts

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
    write('=== Test 33: Date/Time Predicates ==='), nl, nl,
    test_get_time,
    test_now,
    test_today,
    test_date_add,
    test_date_diff,
    test_day_of_week,
    test_date_parts,
    test_time_parts,
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
% 1. get_time
% ============================================================
test_get_time :-
    write('--- get_time ---'), nl,
    run_test(get_time_returns_number,
        (get_time(T), number(T))),
    run_test(get_time_positive,
        (get_time(T), T > 0)),
    run_test(get_time_increasing,
        (get_time(T1), get_time(T2), T2 >= T1)).

% ============================================================
% 2. now
% ============================================================
test_now :-
    write('--- now ---'), nl,
    run_test(now_returns_atom,
        (now(N), atom(N))),
    run_test(now_contains_T_separator,
        (now(N), atom_codes(N, Codes), member(84, Codes))),
    run_test(now_nonempty,
        (now(N), atom_length(N, L), L > 10)).

% ============================================================
% 3. today
% ============================================================
test_today :-
    write('--- today ---'), nl,
    run_test(today_returns_atom,
        (today(T), atom(T))),
    run_test(today_length_10,
        (today(T), atom_length(T, 10))),
    run_test(today_contains_dashes,
        (today(T), atom_codes(T, Codes), member(45, Codes))).

% ============================================================
% 4. date_add
% ============================================================
test_date_add :-
    write('--- date_add ---'), nl,
    run_test(date_add_days,
        (date_add('2025-01-01', 10, days, R), R == '2025-01-11')),
    run_test(date_add_months,
        (date_add('2025-01-15', 2, months, R), R == '2025-03-15')),
    run_test(date_add_years,
        (date_add('2025-06-15', 1, years, R), R == '2026-06-15')),
    run_test(date_add_weeks,
        (date_add('2025-01-01', 2, weeks, R), R == '2025-01-15')),
    run_test(date_add_negative_days,
        (date_add('2025-03-15', -15, days, R), R == '2025-02-28')).

% ============================================================
% 5. date_diff
% ============================================================
test_date_diff :-
    write('--- date_diff ---'), nl,
    run_test(date_diff_days,
        (date_diff('2025-01-01', '2025-01-11', days, D), D == 10)),
    run_test(date_diff_months,
        (date_diff('2025-01-15', '2025-04-15', months, D), D == 3)),
    run_test(date_diff_years,
        (date_diff('2020-01-01', '2025-01-01', years, D), D == 5)),
    run_test(date_diff_negative,
        (date_diff('2025-01-11', '2025-01-01', days, D), D == -10)).

% ============================================================
% 6. day_of_week
% ============================================================
test_day_of_week :-
    write('--- day_of_week ---'), nl,
    run_test(day_of_week_wednesday,
        (day_of_week('2025-01-01', D), D == wednesday)),
    run_test(day_of_week_sunday,
        (day_of_week('2025-01-05', D), D == sunday)),
    run_test(day_of_week_is_atom,
        (day_of_week('2025-06-15', D), atom(D))).

% ============================================================
% 7. date_parts
% ============================================================
test_date_parts :-
    write('--- date_parts ---'), nl,
    run_test(date_parts_basic,
        (date_parts('2025-06-15', Y, M, D), Y == 2025, M == 6, D == 15)),
    run_test(date_parts_january,
        (date_parts('2025-01-01', Y, M, D), Y == 2025, M == 1, D == 1)),
    run_test(date_parts_december,
        (date_parts('2025-12-31', Y, M, D), Y == 2025, M == 12, D == 31)).

% ============================================================
% 8. time_parts
% ============================================================
test_time_parts :-
    write('--- time_parts ---'), nl,
    run_test(time_parts_basic,
        (time_parts('2025-06-15T14:30:45', H, M, S), H == 14, M == 30, S == 45)),
    run_test(time_parts_midnight,
        (time_parts('2025-01-01T00:00:00', H, M, S), H == 0, M == 0, S == 0)),
    run_test(time_parts_from_date,
        (time_parts('2025-06-15', H, M, S), H == 0, M == 0, S == 0)).

:- run_all_tests.
