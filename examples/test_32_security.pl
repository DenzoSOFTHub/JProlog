% Test 32: Security Tests
% ISS-2025-0174 - Regex and XML security

% === Test 1: Regex basic match ===
test_regex_match :-
    re_match('[0-9]+', '12345').

test_regex_no_match :-
    \+ re_match('[0-9]+', 'hello').

% === Test 2: Regex replace (arg order: Pattern, Replacement, Input, Output) ===
test_regex_replace :-
    re_replace('[0-9]+', 'NUM', 'hello123world', Result),
    Result = helloNUMworld.

% === Test 3: Regex with special chars in pattern ===
test_regex_special :-
    re_match('hello\\.world', 'hello.world').

% === Test 4: Invalid regex throws error ===
test_regex_invalid :-
    catch(
        (re_match('[invalid', 'test'), fail),
        _,
        true
    ).

% === Test 5: Regex escape ===
test_regex_escape :-
    re_escape('a.b+c', Escaped),
    re_match(Escaped, 'a.b+c').

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Security Tests ==='), nl.
:- run_test('Regex match', test_regex_match).
:- run_test('Regex no match', test_regex_no_match).
:- run_test('Regex replace', test_regex_replace).
:- run_test('Invalid regex error', test_regex_invalid).
:- write('=== Security Tests Complete ==='), nl.
