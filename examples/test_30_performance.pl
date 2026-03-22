% Test 30: Performance and Debug Tests
% ISS-2025-0172 - Performance optimizations

% === Test 1: sub_atom with all bound args (should be fast) ===
test_sub_atom_all_bound :-
    sub_atom(abcdefghij, 2, 3, 5, cde).

% === Test 2: sub_atom with Before bound ===
test_sub_atom_before_bound :-
    sub_atom(hello, 0, Len, _, Sub),
    Sub = hello,
    Len =:= 5.

% === Test 3: sub_atom with Length bound ===
test_sub_atom_length_bound :-
    findall(Sub, sub_atom(abc, _, 1, _, Sub), Subs),
    length(Subs, N),
    N =:= 3.

% === Test 4: sub_atom enumerate all ===
test_sub_atom_enumerate :-
    findall(Sub, sub_atom(ab, _, _, _, Sub), Subs),
    length(Subs, N),
    N >= 3.  % "", "a", "b", "ab" etc.

% === Test 5: sub_atom with SubAtom bound (search mode) ===
test_sub_atom_search :-
    findall(B, sub_atom(abcabc, B, _, _, abc), Bs),
    Bs = [0, 3].

% === Test 6: Large atom sub_atom ===
test_sub_atom_large :-
    atom_concat(abcdefghijklmnopqrstuvwxyz, abcdefghijklmnopqrstuvwxyz, Big),
    sub_atom(Big, 25, 2, _, za).

% === Test 7: Deterministic queries ===
test_deterministic :-
    atom_length(hello, 5).

% === Test 8: Findall performance ===
test_findall_perf :-
    numlist(1, 100, List),
    findall(X, member(X, List), Xs),
    length(Xs, 100).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Performance Tests (Phase 11) ==='), nl.
:- run_test('sub_atom all bound', test_sub_atom_all_bound).
:- run_test('sub_atom Before bound', test_sub_atom_before_bound).
:- run_test('sub_atom Length bound', test_sub_atom_length_bound).
:- run_test('sub_atom enumerate', test_sub_atom_enumerate).
:- run_test('sub_atom search', test_sub_atom_search).
:- run_test('sub_atom large', test_sub_atom_large).
:- run_test('deterministic query', test_deterministic).
:- run_test('findall 100 elements', test_findall_perf).
:- write('=== Performance Tests Complete ==='), nl.
