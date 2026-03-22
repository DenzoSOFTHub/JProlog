% Test 36: Regex predicates
% Tests: re_match, re_matchsub, re_replace, re_split, re_findall

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
    write('=== Test 36: Regex Predicates ==='), nl, nl,
    test_re_match,
    test_re_matchsub,
    test_re_replace,
    test_re_split,
    test_re_findall,
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
% 1. re_match
% ============================================================
test_re_match :-
    write('--- re_match ---'), nl,
    run_test(match_simple,
        re_match('hello', 'hello world')),
    run_test(match_digits,
        re_match('[0-9]+', 'abc123def')),
    run_test(match_fail,
        \+ re_match('[0-9]+', 'abcdef')),
    run_test(match_anchor_start,
        re_match('^hello', 'hello world')),
    run_test(match_anchor_end,
        re_match('world$', 'hello world')),
    run_test(match_no_start_anchor,
        \+ re_match('^world', 'hello world')).

% ============================================================
% 2. re_matchsub
% ============================================================
test_re_matchsub :-
    write('--- re_matchsub ---'), nl,
    run_test(matchsub_groups,
        (re_matchsub('(\\w+)@(\\w+)', 'user@host', Groups),
         is_list(Groups))),
    run_test(matchsub_no_match_fails,
        \+ re_matchsub('[0-9]+', 'abcdef', _)),
    run_test(matchsub_digits,
        (re_matchsub('([0-9]+)', 'abc123def', Groups),
         is_list(Groups))).

% ============================================================
% 3. re_replace
% ============================================================
test_re_replace :-
    write('--- re_replace ---'), nl,
    run_test(replace_simple,
        (re_replace('world', 'prolog', 'hello world', R),
         R == 'hello prolog')),
    run_test(replace_digits_to_NUM,
        (re_replace('[0-9]+', 'NUM', 'abc123def456', R),
         R == 'abcNUMdefNUM')),
    run_test(replace_to_empty,
        (re_replace('x', '', 'axbxc', R),
         R == 'abc')),
    run_test(replace_no_match_unchanged,
        (re_replace('xyz', 'ABC', 'hello', R),
         R == 'hello')).

% ============================================================
% 4. re_split
% ============================================================
test_re_split :-
    write('--- re_split ---'), nl,
    run_test(split_comma,
        (re_split(',', 'a,b,c', Parts),
         Parts == [a, b, c])),
    run_test(split_whitespace,
        (re_split('\\s+', 'hello world  test', Parts),
         Parts == [hello, world, test])),
    run_test(split_no_match_single,
        (re_split(',', 'hello', Parts),
         Parts == [hello])).

% ============================================================
% 5. re_findall
% ============================================================
test_re_findall :-
    write('--- re_findall ---'), nl,
    run_test(findall_digits,
        (re_findall('[0-9]+', 'abc123def456ghi789', Matches),
         Matches == ['123', '456', '789'])),
    run_test(findall_words,
        (re_findall('[a-z]+', 'Hello World Test', Matches),
         Matches == [ello, orld, est])),
    run_test(findall_no_match_empty,
        (re_findall('[0-9]+', 'abcdef', Matches),
         Matches == [])).

:- run_all_tests.
