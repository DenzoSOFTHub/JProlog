% Test 32: JSON predicates
% Tests: json_parse, json_serialize, json_get, json_set, json_keys, json_member

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
    write('=== Test 32: JSON Predicates ==='), nl, nl,
    test_json_parse,
    test_json_serialize,
    test_json_get,
    test_json_set,
    test_json_keys,
    test_json_member,
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
% 1. JSON Parse
% ============================================================
test_json_parse :-
    write('--- JSON Parse ---'), nl,
    run_test(parse_object,
        (json_parse('{"name":"Alice","age":30}', T), nonvar(T))),
    run_test(parse_number_int,
        (json_parse('42', T), T == 42)),
    run_test(parse_number_float,
        (json_parse('3.14', T), T == 3.14)),
    run_test(parse_string,
        (json_parse('"hello"', T), T == hello)),
    run_test(parse_true,
        (json_parse('true', T), T == @(true))),
    run_test(parse_false,
        (json_parse('false', T), T == @(false))),
    run_test(parse_null,
        (json_parse('null', T), T == @(null))),
    run_test(parse_array,
        (json_parse('[1,2,3]', T), is_list(T), length(T, 3))),
    run_test(parse_nested_object,
        (json_parse('{"user":{"name":"Bob"}}', T), nonvar(T))),
    run_test(parse_empty_object,
        (json_parse('{}', T), T == json([]))),
    run_test(parse_empty_array,
        (json_parse('[]', T), T == [])).

% ============================================================
% 2. JSON Serialize
% ============================================================
test_json_serialize :-
    write('--- JSON Serialize ---'), nl,
    run_test(serialize_number,
        (json_serialize(42, S), atom(S))),
    run_test(serialize_true,
        (json_serialize(@(true), S), S == true)),
    run_test(serialize_null,
        (json_serialize(@(null), S), S == null)),
    run_test(serialize_roundtrip,
        (json_parse('{"x":1}', T), json_serialize(T, S), atom(S))),
    run_test(serialize_array_roundtrip,
        (json_parse('[1,2,3]', T), json_serialize(T, S), atom(S))).

% ============================================================
% 3. JSON Get
% ============================================================
test_json_get :-
    write('--- JSON Get ---'), nl,
    run_test(get_string_field,
        (json_parse('{"name":"Alice","age":30}', J),
         json_get(J, name, V), V == 'Alice')),
    run_test(get_number_field,
        (json_parse('{"name":"Alice","age":30}', J),
         json_get(J, age, V), V == 30)),
    run_test(get_nested_field,
        (json_parse('{"user":{"name":"Bob"}}', J),
         json_get(J, user, U), json_get(U, name, N), N == 'Bob')).

% ============================================================
% 4. JSON Set
% ============================================================
test_json_set :-
    write('--- JSON Set ---'), nl,
    run_test(set_add_field,
        (json_parse('{"name":"Alice"}', J),
         json_set(J, age, 30, J2),
         json_get(J2, age, V), V == 30)),
    run_test(set_overwrite_field,
        (json_parse('{"name":"Alice"}', J),
         json_set(J, name, 'Bob', J2),
         json_get(J2, name, V), V == 'Bob')).

% ============================================================
% 5. JSON Keys
% ============================================================
test_json_keys :-
    write('--- JSON Keys ---'), nl,
    run_test(keys_basic,
        (json_parse('{"a":1,"b":2}', J),
         json_keys(J, Keys), is_list(Keys), length(Keys, 2))),
    run_test(keys_empty,
        (json_parse('{}', J),
         json_keys(J, Keys), Keys == [])).

% ============================================================
% 6. JSON Member
% ============================================================
test_json_member :-
    write('--- JSON Member ---'), nl,
    run_test(member_exists,
        (json_parse('{"x":1,"y":2}', J),
         json_member(J, x, V), V == 1)),
    run_test(member_not_exists,
        (json_parse('{"x":1}', J),
         \+ json_member(J, z, _))).

:- run_all_tests.
